module Parser.Impl where

import SubsAst
import Data.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Control.Monad
import Text.Parsec (ParseError)
import Text.Parsec

-- can change this if you want, but must be an instance of Show and Eq
data CustomParseError = CustomParseError String
                deriving (Show, Eq)



keywords :: [String]
keywords = [ "true", "false", "for", "undefined", "of", "if", "case", "do"]


-- To move removing whitespaces higher during parsing

cParse :: Parser a -> String -> Either ParseError a
cParse p = parse p ""

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           pWhitespaces
           return x

parseWWS :: Parser a -> String -> Either ParseError a
parseWWS p = cParse wrapper
  where
    wrapper = do
        pWhitespaces
        p

pWhitespaces :: Parser ()
pWhitespaces = void $ many $ oneOf " \n\t"

-- ---------------------------------------------------


parseString :: String -> Either CustomParseError Expr
parseString = undefined

-- ------------ EXPR Start ---------------------------

-- pExpr :: Parser Expr <- todo: change for refactored
-- pExpr = do
--   e0 <- try pExpr
--   void $ lexeme $ char ','
--   e1 <- pExpr
--   return (Comma e0 e1)
--   <|> 
--   do try pExpr1

pExpr1 :: Parser Expr
pExpr1 = do
  try pIdentOnly
  <|>
  do try pAssignent
  <|>
  do try pFunCall
  <|>
  do try pTerminal

pExprs :: Parser Expr
pExprs = do
  try pEmpty
  <|>
  do
    e0 <- try pExpr1
    (Array c0) <- pCommaExpr
    return (Array (e0:c0))

pEmpty :: Parser Expr
pEmpty = do
  void $ lexeme $ try eof
  return (Array [])

pCommaExpr :: Parser Expr
pCommaExpr = do
  try pEmpty
  <|>
  do 
    void $ try $ lexeme $ char ','
    e0 <- try pExpr1
    (Array c0) <- pCommaExpr
    return (Array (e0:c0))



-- ---------------  Terminal -----------------------

pTerminal :: Parser Expr
pTerminal = do
  try pNumber 
  <|>
  do try pString
  <|>
  do try pTrue
  <|>
  do try pFalse
  <|>
  do try pUndefined

pParen :: Parser Expr
pParen = undefined

pExpr1' :: Parser Expr
pExpr1' = undefined



-- Terminal -> Number | String | "true" | "false" | "undefined"

pNumber :: Parser Expr
pNumber = do
  n <- lexeme $ many1 digit
  return (Number (read n))

pString :: Parser Expr
pString = lexeme $ do
  h <- firstChar
  t <- many nonFirstChar
  let word = h:t
  if word `notElem` keywords
    then return (String word)
    else fail "String can't be a keyword"
  where
    firstChar = satisfy (\a -> isAscii a)
    nonFirstChar = satisfy (\a -> isAscii  a) -- || a `elem` ["\'", "\"", "\n", "\t", "\\"] how to make it skip those signs?

pTrue :: Parser Expr
pTrue = do
  val <- pWord
  guard (val == "true")
  return TrueConst

pFalse :: Parser Expr
pFalse = do
  val <- pWord
  guard (val == "false")
  return FalseConst

pUndefined :: Parser Expr
pUndefined = do
  val <- pWord
  guard (val == "undefined")
  return Undefined

-- --------------------------------------------------------

-- ---------------  Ident ------------

pIdent :: Parser Expr
pIdent = lexeme $ do
  h <- firstChar
  t <- many nonFirstChar
  let word = h:t
  if word `notElem` keywords
    then return (Var (h:t))
    else fail "Identificator can't be a keyword"
  where 
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')


pIdentOnly :: Parser Expr
pIdentOnly = lexeme $ do
  i0 <- try pIdent
  void $ lexeme $ try eof
  return i0

pAssignent :: Parser Expr
pAssignent = lexeme $ do
  (Var i0) <- try pIdent
  void $ try $ lexeme $ char '='
  e0 <- lexeme $ pExpr1
  return (Assign i0 e0)

pFunCall :: Parser Expr
pFunCall = lexeme $ do
  (Var i0) <- try pIdent
  void $ try $ lexeme $ char '('
  (Array e0) <- pExprs
  void $ lexeme $ char ')'
  return (Call i0 e0)


-- ------------ Utils -----------------------

pWord :: Parser String
pWord = do
  e0 <- lexeme $ many1 $ letters
  return e0
  where
    letters = satisfy (\a -> isLetter a)

-- pIdentOnly :: Parser String -- todo Finish
-- pIdentOnly = undefined