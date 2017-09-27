module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 


n1 = parseWWS pExpr  "123    "
n2 = parseWWS pExpr  "   231321123"
n3 = parseWWS pExpr   "1"

i1 = parseWWS pExpr  "name    ,second"
i2 = parseWWS pExpr  "  n_321"
i3 = parseWWS pExpr  "dsADS_123___"

s1 = parseWWS pExpr  "'@DF!#!   "
s2 = parseWWS pExpr  "'_!!  dsadeqw"
s3 = parseWWS pExpr  "'==}'dsdsa22''222"
-- s4 = parseWWS pString "true"

t1 = parseWWS pExpr  "true"
t2 = parseWWS pExpr  "false"
t3 = parseWWS pExpr  "undefined"

e1 = parseWWS pExpr  "x = 5"
e2 = parseWWS pExpr  "x"
e3 = parseWWS pExpr  "foo ( 12, 32, x)"

fc1 = parseWWS pExpr "foo ( 3, y , 123, 2)"

es1 = parseWWS pExpr  "5,x"
es2 = parseWWS pExpr  " 5 ,   x , 120,undefined"

o1 = parseWWS pExpr "a + b"
o2 = parseWWS pExpr "a +b +b- 3"



testn1 = TestCase $ assertEqual "Number 1" (Right (Number 123)) (n1)
testn2 = TestCase $ assertEqual "Number 2" (Right (Number 231321123)) (n2)
testn3 = TestCase $ assertEqual "Number 3" (Right (Number 1)) (n3)

testi1 = TestCase $ assertEqual "Ident 1" (Right (Comma (Var "name") (Var "second"))) (i1)
testi2 = TestCase $ assertEqual "Ident 2" (Right (Var "n_321")) (i2)
testi3 = TestCase $ assertEqual "Ident 3" (Right (Var "dsADS_123___")) (i3)

tests1 = TestCase $ assertEqual "String 1" (Right (String "@DF!#!   ")) (s1)
tests2 = TestCase $ assertEqual "String 2" (Right (String "_!!  dsadeqw")) (s2)
tests3 = TestCase $ assertEqual "String 3" (Right (String "==}'dsdsa22''222")) (s3)
-- tests4 = TestCase $ assertEqual "String 4" (Right (String "@DF!#!   ")) (s4) -- todo: how to mah against PerseError - impossibru the constructor is private :c

testt1 = TestCase $ assertEqual "True 1" (Right TrueConst) (t1)
testt2 = TestCase $ assertEqual "False 1" (Right FalseConst) (t2)
testt3 = TestCase $ assertEqual "Undefined 1" (Right Undefined) (t3)

teste1 = TestCase $ assertEqual "Expression 1" (Right (Assign "x" (Number 5))) (e1)
teste2 = TestCase $ assertEqual "Expression 2" (Right (Var "x")) (e2)
teste3 = TestCase $ assertEqual "Expression 3" (Right (Call "foo"  [Number 12, Number 32, Var "x"])) (e3)

testfc1 = TestCase $ assertEqual "Function Call 1" (Right (Call "foo" [Number 3,Var "y",Number 123,Number 2])) (fc1)

testes1 = TestCase $ assertEqual "Expressions 1" (Right (Comma (Number 5) (Var "x"))) (es1)
testes2 = TestCase $ assertEqual "Expressions 2" (Right (Comma (Number 5) (Comma (Var "x") (Comma (Var "x") (Comma (Number 120) (Comma (Number 120) Undefined)))))) (es2)

testo1 = TestCase $ assertEqual "Operation 1" (Right (Call "+" [Var "a", Var "b"])) (o1)
testo2 = TestCase $ assertEqual "Operation 1" (Right (Call "+" [Var "a", (Call "+" [Var "b", (Call "-" [Var "b", Number 3])])])) (o2)

tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3],
          TestLabel "Ident" $ TestList [testi1, testi2, testi3],
          TestLabel "String" $ TestList [tests1, tests2, tests3],
          TestLabel "Terminal" $ TestList [testt1, testt2, testt3],
          TestLabel "Expressions" $ TestList [teste1, teste2, teste3, testfc1, testes1, testes2, testo1, testo2]]



main = runTestTT tests

