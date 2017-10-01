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

pExpr :: Parser Expr
pExpr = 
  try ( do
    e0 <- pExpr1
    e1 <- (pCommaExpr e0)
    return e1 )
  <|> 
  do pExpr1

pExpr1 :: Parser Expr
pExpr1 =
  try ( do
    e0 <- pExpr2
    (pOperation e0))
  <|>
  do pExpr2

pOperation :: Expr -> Parser Expr
pOperation e0 = do
  void $ lexeme $ char '+'
  e1 <- pExpr1
  return (Call "+" [e0,e1])
  <|>
  do
    void $ lexeme $ char '-'
    e1 <- pExpr1
    return (Call "-" [e0,e1])
  <|>
  do
    void $ lexeme $ char '*'
    e1 <- pExpr1
    return (Call "*" [e0,e1])
  <|>
  do
    void $ lexeme $ char '%'
    e1 <- pExpr1
    return (Call "%" [e0,e1])
  <|>
  do
    void $ lexeme $ char '<'
    e1 <- pExpr1
    return (Call "<" [e0,e1])
  <|>
  do
    void  $ lexeme $ string "==="
    e1 <- pExpr1
    return (Call "===" [e0,e1])


pExpr2 :: Parser Expr
pExpr2 = 
  try ( do
    i0 <- pIdent
    (pFunCall i0))
  <|>
  try ( do
    i0 <- pIdent
    (pAssignent i0))  
  <|>
    try ( do
    i0 <- pIdent
    (pIdentOnly i0))  
 <|>
  try ( do
    void $ lexeme $ char '['
    e0 <- pExprs
    void $ lexeme $ char ']'
    return e0
    )
 <|>
   try ( do
    void $ lexeme $ char '['
    e0 <- pArrayFor
    void $ lexeme $ char ']'
    return e0
    )
 <|>
    try ( do
    void $ lexeme $ char '('
    e0 <- pExpr
    void $ lexeme $ char ')'
    return e0
    )
 <|>
 do pTerminal
  
pExprs :: Parser Expr
pExprs = try ( do
    e0 <- pExpr1
    (Array e1) <- pArguments 
    return (Array (e0:e1)))
  <|>
  try (do
    e0 <- pExpr1
    return e0)
  <|>
  do 
    pEmpty (Array [])

pArguments :: Parser Expr
pArguments = try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    (Array e2) <- pArguments
    return (Array (e1:e2)))
  <|>
  try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    return (Array [e1]))
  <|>
  do return (Array [])


pEmpty :: Expr -> Parser Expr
pEmpty e0 = return e0 

pCommaExpr :: Expr -> Parser Expr
pCommaExpr e0 = try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    e2 <- (pCommaExpr e1)
    return (Comma e0 (Comma e1 e2)))
  <|>
  try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    return (Comma e0 e1))


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


-- Terminal -> Number | String | "true" | "false" | "undefined"

pNumber :: Parser Expr
pNumber = 
  try (do t <- (lexeme $ many1 digit) 
          notFollowedBy (char '.')
          if ((length t) > 8) then fail "Number too large"
                              else return (Number (listToInt (t))))
  <|>
  do h <- char '-'
     t <- (lexeme $ many1 digit)
     notFollowedBy (char '.')
     if ((length t) > 8) then fail "Number too large"
                         else return (Number (-1 * (listToInt (t))))


pString :: Parser Expr
pString = lexeme $ do
  h <- firstChar
  t <- many nonFirstChar
  if( last t == '\'') then return (String (init (tail (h:t))))
                      else fail "Badly formed string"
  where
    firstChar = satisfy (\a -> a=='\'')
    nonFirstChar = satisfy (\a -> isAscii  a) -- || a `elem` ["\'", "\"", "\n", "\t", "\\"] how to make it skip those signs?


pTrue :: Parser Expr
pTrue = do
  void $ lexeme $ string "true"
  return TrueConst

pFalse :: Parser Expr
pFalse = do
  void $ lexeme $ string "false"
  return FalseConst

pUndefined :: Parser Expr
pUndefined = do
  void $ lexeme $ string "undefined"
  return Undefined

-- --------------------------------------------------------

-- ---------------  Ident ------------

pIdent :: Parser Expr
pIdent = lexeme $ do
  h <- firstChar
  t <- lexeme $ many nonFirstChar
  let word = h:t
  if word `notElem` keywords
    then return (Var (h:t))
    else fail "Can't user keyword as identificator"
  where 
    firstChar = satisfy (\a -> isLetter a)
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')


pIdentOnly :: Expr -> Parser Expr
pIdentOnly i0 = return i0

pAssignent :: Expr -> Parser Expr
pAssignent (Var i0) = lexeme $ do
  void $ lexeme $ char '='
  e0 <- lexeme $ pExpr1
  return (Assign i0 e0)
pAssignent _ = fail "error"

pFunCall :: Expr -> Parser Expr
pFunCall (Var i0) = lexeme $ do
  void $ lexeme $ char '('
  (Array e0) <- lexeme $ pExprs
  void $ lexeme $ char ')'
  return (Call i0 e0)
pFunCall _ = fail "Function call wrong ident error"



-- --------- Array Compr ---------------------

pArrayFor :: Parser Expr
pArrayFor = do
  void $ lexeme $ try $ string "for"
  void $ lexeme $ char '('
  (Var i0) <- pIdent
  void $ lexeme $ string "of"
  e0 <- pExpr1
  void $ lexeme $ char ')'
  a0 <- pArrayCompr
  return (Compr (ACFor i0 e0 a0))

pArrayIf :: Parser ArrayCompr
pArrayIf = do
  void $ lexeme $ try $ string "if"
  void $ lexeme $ char '('
  e0 <- pExpr1
  void $ lexeme $ char ')'
  a0 <- pArrayCompr
  return (ACIf e0 a0)

pArrayCompr :: Parser ArrayCompr
pArrayCompr = 
  do 
    e0 <- try pExpr1
    return (ACBody e0)
  <|>
  do 
    (Compr a0) <- try pArrayFor
    return a0
  <|>
  do try pArrayIf





-- ------------ Utils -----------------------
listToInt':: [Char] ->Int -> Int 
listToInt' [] _ = 0
listToInt' (h:t) n = (10^(n-1) * digitToInt h ) + (listToInt' t (n-1))

listToInt:: [Char] -> Int 
listToInt l = listToInt' l (length l)

pWord :: String -> Parser String
pWord a = do
  word <- lexeme $ many1 $ letters
  if word == a then return word
    else fail "pWord Error"
  where
    letters = satisfy (\a -> isLetter a)

-- pIdentOnly :: Parser String -- todo Finish
-- pIdentOnly = undefined