module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Int
          | ExprVar    String
          | ExprOper   String Expr Expr
          | ExprFunc   String [Expr] 
          deriving Show

-- Suger data for types that will be desugered into Stat types
data Suger = StatFor ExprDecls Expr ExprDecls Stat

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)

data ExprDecls = EDExpr Expr |
                 EDDecl Decl |
                 EDExprs Expr ExprDecls |
                 EDDecls Decl ExprDecls |
                 EDEmpty -- Empty can be used to give the default when using this parser in option
          deriving Show


pExprDecls :: Parser Token ExprDecls
pExprDecls = EDExprs <$> pExpr <* symbol Comma <*> pExprDecls <|>
             EDDecls <$> pDecl <* symbol Comma <*> pExprDecls <|>
             EDExpr  <$> pExpr <|>
             EDDecl  <$> pDecl


pSuger :: Parser Token Suger -- Doing it this way you could more easily add more sugar in the future. 
pSuger = StatFor <$> (symbol KeyFor *> symbol POpen *> option pExprDecls EDEmpty <* sSemi) <*> pExprSemi <*> (option pExprDecls EDEmpty <* symbol PClose) <*> pStat

-- The for consists of 4 parts. The initial expressions/declerations, a condition, the repeating decleration/expresion and the other instructions. 
-- I can construct another [Stat] with first the initial declerations then a WhileStat wich copies the condition expr and then just put the final expr/decls at the end of the already existing [Stat]
deSugar :: Suger -> Stat
deSugar (StatFor forHead condition repeat forBody) = StatBlock (exprDeclsToList forHead ++ [StatWhile condition (StatBlock (fromStatBlock forBody ++ exprDeclsToList repeat))])


exprDeclsToList :: ExprDecls -> [Stat] -- Takes a ExprDecls and turns it into a StatBlock with declerations and expressions. This is needed to desugar the for.
exprDeclsToList EDEmpty =  []
exprDeclsToList (EDExpr expr) = [StatExpr expr]
exprDeclsToList (EDDecl decl) = [StatDecl decl]
exprDeclsToList (EDDecls decl decls) = StatDecl decl : exprDeclsToList decls
exprDeclsToList (EDExprs expr exprs) = StatExpr expr : exprDeclsToList exprs

fromStatBlock :: Stat -> [Stat]
fromStatBlock (StatBlock stats) = stats
fromStatBlock a = error ("You tried to get the stats from a non block stat: " ++ show a)

data A = A ExprDecls Expr ExprDecls
     deriving Show

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi <|>
         StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse <|>
         StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat <|>
         deSugar    <$> pSuger <|> -- For loop production 
         StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi <|>
         pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

fact :: Parser Token Expr
fact =
     ExprConst <$> sConst <|>
     (ExprFunc <$> sLowerId <*> parenthesised (option (sTokenTuple pExpr) []) <<|> ExprVar <$> sLowerId ) <|>
     parenthesised pExpr

type Op a = (Token, a -> a -> a)

gen :: [Op a] -> Parser Token a -> Parser Token a
gen ops p = chainl p (choice (map f ops))
     where f (s, c) = const c <$> symbol s
     
pExprL :: Parser Token Expr 
pExprL = foldr gen fact [orOp, andOp, xorOp, eqornotOp, comprOp, addisOp, multisOp]

pExpr :: Parser Token Expr -- chainr because its right associative. 
pExpr = chainr pExprL (ExprOper <$> sAssign)

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pExprSemi :: Parser Token Expr
pExprSemi = pExpr <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}

sTokenTuple, sSemicList :: Parser Token a -> Parser Token [a]
sTokenTuple p = listOf p sComma
sSemicList p = listOf p sSemi

multisOp :: [(Token, Expr -> Expr -> Expr)]
multisOp = [(Operator "*", ExprOper "*"), (Operator "/", ExprOper "/"), (Operator "%", ExprOper "%")]

addisOp :: [(Token, Expr -> Expr -> Expr)]
addisOp = [(Operator "+", ExprOper "+"), (Operator "-", ExprOper "-")]

comprOp :: [(Token, Expr -> Expr -> Expr)]
comprOp = [(Operator "<=", ExprOper "<="),(Operator "<", ExprOper "<"),(Operator ">=", ExprOper ">="),(Operator ">", ExprOper ">")]

eqornotOp :: [(Token, Expr -> Expr -> Expr)]
eqornotOp = [(Operator "!=", ExprOper "!="),(Operator "==", ExprOper "==")]

xorOp :: [(Token, Expr -> Expr -> Expr)]
xorOp = [(Operator "^", ExprOper "^")]

andOp :: [(Token, Expr -> Expr -> Expr)]
andOp = [(Operator "&&", ExprOper "&&")]

orOp :: [(Token, Expr -> Expr -> Expr)]
orOp = [(Operator "||", ExprOper "||")]

assignOp :: [(Token, Expr -> Expr -> Expr)]
assignOp = [(Operator "=", ExprOper "=")]

-- Order of operation:
--  "*" 
--  "/" 
--  "%" 

--  "+" 
--  "-"

--  "<="
--  "<"
--  ">=" 
--  ">"

--  "==" 
--  "!="

--  "^"

--  "&&"

--  "||"

--   "="


