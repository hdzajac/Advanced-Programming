module Parser.Impl where

import SubsAst
import Data.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Control.Monad
import Text.Parsec


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


parseString :: String -> Either ParseError Expr
parseString = parseWWS pStart

-- ------------ EXPR Start ---------------------------

pStart :: Parser Expr
pStart = do
  e0 <- pExpr
  void $ eof
  return e0

pExpr :: Parser Expr
pExpr = 
  try ( do
    e0 <- pExpr1
    pCommaExpr e0)
  <|> 
  pExpr1

pExpr1 :: Parser Expr
pExpr1 =
  do 
    r0 <- try pRelationOp
    return r0
  <|>
  pExpr2

pExpr2 :: Parser Expr
pExpr2 = 
  try ( do
    i0 <- pIdent
    pFunCall i0)
  <|>
  try ( do
    i0 <- pIdent
    pAssignent i0)  
  <|>
  do
    try pIdent  
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
 pTerminal
  
pExprs :: Parser Expr
pExprs = try ( do
    e0 <- pExpr1
    (Array e1) <- pArguments 
    return (Array (e0:e1)))
  <|>
    try pExpr1
  <|>
    pEmpty (Array [])

pArguments :: Parser Expr
pArguments = try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    (Array e2) <- pArguments
    return (Array (e1:e2)))
  <|>
    return (Array [])


pEmpty :: Expr -> Parser Expr
pEmpty = return

pCommaExpr :: Expr -> Parser Expr
pCommaExpr e0 = try ( do
    void $ lexeme $ char ','
    e1 <- pExpr
    return (Comma e0 e1))
  <|>
  try ( do
    void $ lexeme $ char ','
    e1 <- pExpr1
    return (Comma e0 e1))


-- -------------- Operations -------------------


pRelationOp :: Parser Expr
pRelationOp = pSubSumOp `chainl1` pCompOp

pSubSumOp :: Parser Expr
pSubSumOp = pTerm `chainl1` pAddOp

pTerm :: Parser Expr
pTerm = pExpr2 `chainl1` pMulOp

pCompOp :: Parser (Expr -> Expr -> Expr)
pCompOp = do 
  a <- lexeme (string "===" <|> string "<")
  return (functionCall a)


pAddOp :: Parser (Expr -> Expr -> Expr)
pAddOp = do 
  a <- lexeme (string "+" <|> string "-")
  return (functionCall a)

pMulOp :: Parser (Expr -> Expr -> Expr)
pMulOp = do 
  a <- lexeme (string "*" <|> string "%")
  return (functionCall a)

functionCall :: String->Expr -> Expr -> Expr
functionCall s e1 e2 = Call s [e1,e2]


-- ---------------  Terminal -----------------------

pTerminal :: Parser Expr
pTerminal = do
  try pNumber 
  <|>
    try pString
  <|>
    try pTrue
  <|>
    try pFalse
  <|>
    pUndefined


-- Terminal -> Number | String | "true" | "false" | "undefined"

pNumber :: Parser Expr
pNumber =
  try (do t <- (lexeme $ many1 digit)
          notFollowedBy (char '.')
          if length t > 8 then fail "Number too large"
                              else return (Number (listToInt (t))))
  <|>
  do _ <- char '-'
     t <- (lexeme $ many1 digit)
     notFollowedBy (char '.')
     if length t > 8 then fail "Number too large"
                         else return (Number (-1 * (listToInt (t))))


pString :: Parser Expr
pString = lexeme $ do
  void $ char '\''
  pString1 ""


pString1 :: String -> Parser Expr
pString1 c0 = do
  void $ try $ char '\\'
  pEscape c0
  <|> do
    void $ try $ char '\''
    return (String c0)
  <|> do
    void $ try $ oneOf "\t"
    pString1 (c0)
  <|> do
    c1 <- anyChar
    pString1 (c0 ++ [c1])

pEscape :: String -> Parser Expr
pEscape c0 = do
  void $ try $ char 't'
  pString1 (c0 ++ "\t")
  <|> do
    void $ try $ char 'n'
    pString1 (c0 ++ "\n")
  <|> do
    void $ try $ char '\\'
    pString1 (c0 ++ "\\")
  <|> do
    void $ try $ char '\n'
    pString1 (c0)
  <|> do
    void $ char '\''
    pString1 (c0 ++ "\'")



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
  void $ string "of"
  void $ many1 $ oneOf " \n\t"
  e0 <- pExpr1
  void $ lexeme $ char ')'
  a0 <- pArrayCompr
  return (Compr (ACFor i0 e0 a0))

pArrayIf :: Parser ArrayCompr
pArrayIf = do
  void $ try $ string "if"
  void $ many1 $ oneOf " \n\t"
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
    letters = satisfy (\x -> isLetter x)