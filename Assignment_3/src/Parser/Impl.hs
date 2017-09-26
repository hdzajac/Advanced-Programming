module Parser.Impl where

import SubsAst
import Data.Char
import Text.Parsec.String
import Control.Applicative
import Control.Monad
import Text.Parsec (ParseError)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Text.Parsec as P

-- can change this if you want, but must be an instance of Show and Eq
data CustomParseError = CustomParseError String
                deriving (Show, Eq)

type KeyStore = Map String String -- map of keywords


keywords :: KeyStore
keywords = keyStore
  where keyStore =
          Map.fromList [ ("true", "true")
                       , ("false", "false")
                       , ("for", "for")
                       , ("undefined", "undefined")
                       , ("of", "of")
                       , ("if", "if")
                       ]


-- To move removing whitespaces higher during parsing

cParse :: Parser a -> String -> Either ParseError a
cParse p = P.parse p ""

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
pWhitespaces = void $ P.many $ P.oneOf " \n\t"

-- ---------------------------------------------------


parseString :: String -> Either CustomParseError Expr
parseString = undefined

-- ------------ EXPR Start ---------------------------

pExpr :: Parser Expr
pExpr = pExpr' <|> pExpr1


pExpr' :: Parser Expr
pExpr' = do
  e0 <- pExpr
  void $ lexeme $ P.char ','
  e1 <- pExpr
  return (Comma e0 e1)


pExpr1 :: Parser Expr
pExpr1 = pTerminal <|> pIdentWrap <|> pParen <|> pExpr1'

pTerminal :: Parser Expr
pTerminal = pNumber <|> pString <|> pTrue <|> pFalse <|> pUndefined

pIdentWrap :: Parser Expr
pIdentWrap = undefined

pParen :: Parser Expr
pParen = undefined

pExpr1' :: Parser Expr
pExpr1' = undefined



-- Terminal -> Number | String | "true" | "false" | "undefined"

pNumber :: Parser Expr
pNumber = do
  n <- lexeme $ P.many1 P.digit
  return (Number (read n))

pString :: Parser Expr
pString = lexeme $ do
  h <- firstChar
  t <- P.many nonFirstChar
  guard (case lookup (h:t) (Map.assocs keywords) of
                                    Nothing -> True
                                    Just _ -> False)
  return (String (h:t))
  where
    firstChar = P.satisfy (\a -> isAscii a)
    nonFirstChar = P.satisfy (\a -> isAscii  a) -- || a `elem` ["\'", "\"", "\n", "\t", "\\"] how to make it skip those signs?

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

-- ---------------  pIdentWrap -> Ident Ident'


pIdent :: Parser Expr
pIdent = lexeme $ do
  h <- firstChar
  t <- P.many nonFirstChar
  -- guard () dunno what i wanted to write
  return (Var (h:t))
  where 
    firstChar = P.satisfy (\a -> isLetter a)
    nonFirstChar = P.satisfy (\a -> isDigit a || isLetter a || a == '_')

pIdent' :: Parser Expr
pIdent' = undefined -- pEmpty -- <|> pAssignent <|> pFunCall todo implement / find out how to represent empty production, cause pEmpty doesn't really work



-- ------------ Utils -----------------------

pWord :: Parser String
pWord = do
  e0 <- lexeme $ P.many1 $ letters
  return e0
  where
    letters = P.satisfy (\a -> isLetter a)

-- pEmpty :: Parser String -- todo Finish
-- pEmpty = undefined