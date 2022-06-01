module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Either


newtype Error = Error String

-- The types that we generate for each datatype: Our type variables for the algebra
type C = Either Error Code                      -- Class
type M = M.Map String Int -> (Either Error Code, [String])         -- Member
type S = M.Map String Int -> M.Map String Int -> (Code, [String])         -- Statement
type E = M.Map String Int -> M.Map String Int -> (ValueOrAddress -> Code) -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = (codeClass, codeMember, codeStatement, codeExpr)

codeClass :: String -> [M] -> C
codeClass c ms = let
                    names = concatMap (\m -> snd (m M.empty)) ms
                    n = length names
                    heap = M.fromList (zip names [-(n)..(-1)])
                    heapSpace = [LDC n, LDR HP, ADD, STR HP]  -- I need to make space on the heap for the global vars.
                    result = concatMap (\m -> fromRight [] $ fst (m heap)) ms
                 in Right (heapSpace ++ [Bsr "main", HALT] ++ result)


codeMember :: (Decl -> M, Type -> String -> [Decl] -> S -> M)
codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl (Decl _ name) heap = (Right [] ,[name])

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t name ps s heap = let  (_, vars) = s heap M.empty -- Get the variables declared in the method. As we don't use stats, it doesn't get evaluated and no errors happen.
                                      local_env = M.fromList (zip vars [1..] )  -- Map the addresses to where they are on the stack.
                                      k = length vars

                                      params = map (\(Decl _ name) -> name) ps
                                      n = length params
                                      params_env = M.fromList (zip params [-(n + 1)..(-2)]) -- [.... -4, -3, -2] starts low because first params are fardest away. We use -(n + 1) 

                                      env = M.union (M.union local_env params_env) (M.fromList [("void", n)]) -- The void is needed for return statements. You can never have a variable called void. Like this we don't need another enviroment just for keeping track of the number of parameters.
                                      (code, _) = s heap env

                                  in
                                      case t of
                                          TypeVoid      -> (Right ([LABEL name ,LINK k]  ++ code ++  [LDC 0, STR RR,UNLINK] ++ cleanup n ++ [RET]),[]) -- Return 0 if TypeVoid
                                          (TypePrim _)  -> (Right ([LABEL name, LINK k] ++ code ++ cleanup n ++ [RET]),[]) -- Assume return is already set.
                                          (TypeObj _)   -> (Left (Error "Returning objects is not implemented."), [])
          where
            cleanup :: Int -> Code
            cleanup n = [STS (-n), AJS (-(n - 1))]


codeStatement :: (Decl -> S, E -> S, E -> S -> S -> S, E -> S -> S, E -> S, [S] -> S)
codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl _ name) heap env = ([], [name])

    fStatExpr :: E -> S
    fStatExpr e heap env = (e heap env Value ++ [pop], [])

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 heap env = (c ++ [BRF (n1 + 2)] ++ fst (s1 heap env) ++ [BRA n2] ++ fst (s2 heap env),
                                snd (s1 heap env) ++ snd (s2 heap env))
        where
            c = e heap env Value
            (n1,n2) = (codeSize $ fst $ s1 heap env, codeSize $ fst $ s2 heap env)

    fStatWhile :: E -> S -> S
    fStatWhile e s1 heap env = ([BRA n] ++ fst (s1 heap env) ++ condition ++ [BRT (-(n + k + 2))], snd $ s1 heap env)
                                where
                                    condition = e heap env Value
                                    (n, k) = (codeSize $ fst $ s1 heap env , codeSize condition)

    fStatReturn :: E -> S -- We need the number of arguments for the cleanup. To keep things more simple I store it as "void" in the enviroment because you can never have a variable called void. This way I don't have to pass down another enviroment.
    fStatReturn e heap env = (e heap env Value ++ [STR RR, UNLINK] ++ cleanup (env M.! "void") ++ [RET], []) -- (e Value env ++ [STR R3], env)
     where
      cleanup :: Int -> Code
      cleanup n = [STS (-n), AJS (-(n - 1))]

    fStatBlock :: [S] -> S
    fStatBlock stats heap env = (concatMap (\s -> fst $ s heap env ) stats, concatMap (\s -> snd $ s heap env) stats)

codeExpr :: (Int -> E, String -> E, String -> E -> E -> E, String -> [E] -> E)
codeExpr = (fExprCon, fExprVar, fExprOp, fExprFunc)
  where
    fExprFunc :: String -> [E] -> E
    fExprFunc "print" args heap env va = concatMap (\a -> a heap env va ++ [TRAP 0]) args ++ [LDC 0]
    fExprFunc "printc" args heap env va = concatMap (\a -> a heap env va ++ [TRAP 1]) args ++ [LDC 0]
    fExprFunc "input" args heap env va = [TRAP 10]
    fExprFunc "inputc" args heap env va = [TRAP 11]
    fExprFunc name args heap env va = concatMap (\a -> a heap env va) args ++ [Bsr name, LDR RR]

    fExprCon :: Int -> E
    fExprCon n heap env va = [LDC n]

    fExprVar :: String -> E
    fExprVar varname heap env va =  if varname `M.member` env
                                        then case va of
                                                    Value    ->  [LDL  (env M.! varname)]
                                                    Address  ->  [LDLA (env M.! varname)]
                                        else case va of
                                                    Value    ->  [LDR HP, LDH (heap M.! varname)]  -- I get the values from the heap by first pushing the HP and then using LDH
                                                    Address  ->  [LDR HP, LDAA (heap M.! varname)] -- I get the addresses of values on the heap by first pushing the HP and then using LDAA. 
                                                                  -- Using the heap like this assumes the HP does not change but with the current implementation it doesn't.

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 heap env va = e2 heap env Value ++ [LDS 0] ++ e1 heap env Address ++ [STA 0]
    fExprOp "||" e1 e2 heap env va =   -- Lazy Or
        e1 heap env Value ++            -- Push e1
        [BRT (sizeOf2 + 6)] ++          -- If its true jump over e2 and false to load true
        e2 heap env Value ++            -- Push e2
        [BRT 4] ++                      -- If its true jump over false to load True 
        [LDC 0, BRA 2] ++                -- Load false and jump over load true
        [LDC (-1)]
      where sizeOf2 = codeSize (e2 heap env Value)

    fExprOp "&&" e1 e2 heap env va =    -- Lazy And
        e1 heap env Value ++            -- Push e1
        [BRF (sizeOf2 + 2)] ++          -- If its false jump over e2 and true to load false 
        e2 heap env Value ++ [BRT 4] ++ -- Push e2
                                        -- If its true jump over false to load True 
        [LDC 0, BRA 2] ++               -- Load false and jump over load true
        [LDC (-1)]
        where sizeOf2 = codeSize (e2 heap env Value)

    fExprOp op  e1 e2 heap env va = e1 heap env Value ++ e2 heap env Value ++ [opCodes M.! op] -- Other operations
      where
        opCodes :: M.Map String Instr
        opCodes = M.fromList [
                               ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD),
                               ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ),
                               ("!=", NE), ("||", OR), ("^", XOR)
                             ]



-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show


