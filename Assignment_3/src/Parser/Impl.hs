module Parser.Impl where

import SubsAst
import Data.Char
import Text.Parsec.String
import qualified Text.Parsec as P

-- can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)

parseString :: String -> Either ParseError Expr
parseString = undefined

int :: Parser Expr
int = do
  n <- P.many1 P.digit
  return (Number (read n))

ident :: Parser Expr
ident = do
  h <- firstChar
  t <- P.many nonFirstChar
  return (Var (h:t))
  where 
    firstChar = P.satisfy (\a -> isLetter a)
    nonFirstChar = P.satisfy (\a -> isDigit a || isLetter a || a == '_')