module CSharpLex where
import System.IO (isEOF)
import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn | KeyFor 
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | Comment   String
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = filter (not . isComment) <$> (lexWhiteSpace *>  greedy (lexToken <* lexWhiteSpace) <* eof)
-- Just filter out the comments before moving on.


lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexConstInt,
               lexComment,
               lexTerminal,
               lexEnum StdType stdTypes,
               lexEnum Operator operators,
               lexLowerId,
               lexUpperId
             ]


lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyFor     , "for"   )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = [ "*", "/", "%","+", "-", "<=", "<", ">=", ">", "==", "!=", "^", "&&", "||", "="]


lexConstChar :: Parser Char Token
lexConstChar = ConstInt . ord <$> pack (symbol '\'') anySymbol (symbol '\'')


lexConstBool :: Parser Char Token
lexConstBool = ConstInt . (\x -> if x == "true" then (-1) else 0) <$> choice [token "false", token "true"]

-- Because there are only numbers anyways lexing constant chars and constant bools to ConstInts right away made the most sense to me.

lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit) <|> lexConstChar <|> lexConstBool

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

--- I decided to make the comments a token. This way we can just easily filter them out at the end of the lexing. This was the most elegant way I could make it.
--- I also figured that someone might want to have comments as a token in the future for some reason.

--- I parse the comments by first looking for // and then it accepts any Char that is not an end of line till and including an end of line. 

lexComments :: Parser Char [Token]
lexComments = greedy lexComment

lexComment :: Parser Char Token
lexComment = Comment . concat <$> sequence [token "//", greedy notEndOfLine, greedy endOfLine]

notEndOfLine :: Parser Char Char
notEndOfLine = satisfy (`notElem` lineEndings)

endOfLine :: Parser Char Char
endOfLine = satisfy (`elem` lineEndings)

lineEndings :: [Char]
lineEndings = "\n\r"

isComment :: Token -> Bool
isComment t = case t of
    Comment _ -> True
    _         -> False 

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = (\(StdType x) -> x) <$> satisfy isStdType
  where isStdType (StdType _) = True
        isStdType _           = False

sUpperId :: Parser Token String
sUpperId = (\(UpperId x) -> x) <$> satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token String
sLowerId = (\(LowerId x) -> x) <$> satisfy isLowerId
  where isLowerId (LowerId _) = True
        isLowerId _           = False

sConst :: Parser Token Int
sConst  = (\(ConstInt x) -> x) <$> satisfy isConst
  where isConst (ConstInt  _) = True
        isConst _             = False

sOperator :: Parser Token String
sOperator = (\(Operator x) -> x) <$> satisfy isOperator
  where isOperator (Operator _) = True
        isOperator _            = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

sComma :: Parser Token Token
sComma =  symbol Comma

sMulti :: Parser Token String 
sMulti = (\(Operator x) -> x) <$> satisfy isMulti
  where 
       isMulti (Operator "*") = True
       isMulti (Operator "/") = True
       isMulti (Operator "%") = True
       isMulti _            = False

sAddi:: Parser Token String 
sAddi = (\(Operator x) -> x) <$> satisfy isMulti
  where 
       isMulti (Operator "+") = True
       isMulti (Operator "-") = True
       isMulti _            = False

sAssign:: Parser Token String 
sAssign = (\(Operator x) -> x) <$> satisfy isAssign 
         where 
              isAssign (Operator "=") = True
              isAssign _            = False