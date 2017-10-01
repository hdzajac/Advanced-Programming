module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 


n1 = parseWWS pStart  "123    "
n2 = parseWWS pStart  "   2311123"
n3 = parseWWS pStart   "1"
n4 = parseWWS pStart   "   1"
n5 = parseWWS pStart " 1 "
n6 = parseWWS pStart   "1"


i1 = parseWWS pStart  "name    ,second"
i2 = parseWWS pStart  "  n_321"
i3 = parseWWS pStart  "dsADS_123___"

s1 = parseWWS pString  "'@DF!#!   '"
s2 = parseWWS pString  "'_!!  dsadeqw'"
s3 = parseWWS pString  "\'==}\'dsdsa22\'\'222\'"
s4 = parseWWS pString  "' '"
s5 = parseWWS pString  "\'\t \' '\n\'"
s6 = parseWWS pString  "'   '"

-- s4 = parseWWS pString "true"

t1 = parseWWS pStart  "true"
t2 = parseWWS pStart  "false"
t3 = parseWWS pStart  "undefined"

e1 = parseWWS pStart  "x = 5"
e2 = parseWWS pStart  "x"
e3 = parseWWS pStart  "foo ( 12, 32, x)"

fc1 = parseWWS pStart "foo ( 3, y , 123, 2)"

es1 = parseWWS pStart  "5,x"
es2 = parseWWS pStart  " 5 ,   x , 120,undefined"

o1 = parseWWS pStart "a + b"
o2 = parseWWS pStart "a +b +b- 3"
o3 = parseWWS pStart "a === b "

a1 = parseWWS pStart "[ for ( x of ( 3 + 3 ) ) y ]"



testn1 = TestCase $ assertEqual "Number 1" (Right (Number 123)) (n1)
testn2 = TestCase $ assertEqual "Number 2" (Right (Number 2311123)) (n2)
testn3 = TestCase $ assertEqual "Number 3" (Right (Number 1)) (n3)
testn4 = TestCase $ assertEqual "Number 4" (Right (Number 1)) (n4)
testn5 = TestCase $ assertEqual "Number 5" (Right (Number 1)) (n5)
testn6 = TestCase $ assertEqual "Number 6" (Right (Number 1)) (n6)

testi1 = TestCase $ assertEqual "Ident 1" (Right (Comma (Var "name") (Var "second"))) (i1)
testi2 = TestCase $ assertEqual "Ident 2" (Right (Var "n_321")) (i2)
testi3 = TestCase $ assertEqual "Ident 3" (Right (Var "dsADS_123___")) (i3)

tests1 = TestCase $ assertEqual "String 1" (Right (String "@DF!#!   ")) (s1)
tests2 = TestCase $ assertEqual "String 2" (Right (String "_!!  dsadeqw")) (s2)
tests3 = TestCase $ assertEqual "String 3" (Right (String "==}'dsdsa22''222")) (s3)
tests4 = TestCase $ assertEqual "String 4" (Right (String " ")) (s4)
tests5 = TestCase $ assertEqual "String 5" (Right (String "")) (s5)
tests6 = TestCase $ assertEqual "String 6" (Right (String "   ")) (s6)
-- tests4 = TestCase $ assertEqual "String 4" (Right (String "@DF!#!   ")) (s4) -- todo: how to mah against PerseError - impossibru the constructor is private :c

testt1 = TestCase $ assertEqual "True 1" (Right TrueConst) (t1)
testt2 = TestCase $ assertEqual "False 1" (Right FalseConst) (t2)
testt3 = TestCase $ assertEqual "Undefined 1" (Right Undefined) (t3)

teste1 = TestCase $ assertEqual "Expression 1" (Right (Assign "x" (Number 5))) (e1)
teste2 = TestCase $ assertEqual "Expression 2" (Right (Var "x")) (e2)
teste3 = TestCase $ assertEqual "Expression 3" (Right (Call "foo"  [Number 12, Number 32, Var "x"])) (e3)

testfc1 = TestCase $ assertEqual "Function Call 1" (Right (Call "foo" [Number 3,Var "y",Number 123,Number 2])) (fc1)

testes1 = TestCase $ assertEqual "Expressions 1" (Right (Comma (Number 5) (Var "x"))) (es1)
testes2 = TestCase $ assertEqual "Expressions 2" (Right (Comma (Number 5) (Comma (Var "x") (Comma (Number 120) Undefined)))) (es2)

testo1 = TestCase $ assertEqual "Operation 1" (Right (Call "+" [Var "a", Var "b"])) (o1)
testo2 = TestCase $ assertEqual "Operation 2" (Right (Call "-" [Call "+" [Call "+" [Var "a",Var "b"],Var "b"],Number 3])) (o2)
testo3 = TestCase $ assertEqual "Operation 3" (Right (Call "===" [Var "a",Var "b"])) (o3)

testa1 = TestCase $ assertEqual "Array Comprhenesion 1" (Right (Compr (ACFor "x" (Call "+" [Number 3,Number 3]) (ACBody (Var "y"))))) (a1)


tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3, testn4, testn5, testn6],
          TestLabel "Ident" $ TestList [testi1, testi2, testi3],
          TestLabel "String" $ TestList [tests1, tests2, tests3, tests4, tests5, tests6],
          TestLabel "Terminal" $ TestList [testt1, testt2, testt3],
          TestLabel "Expressions" $ TestList [teste1, teste2, teste3, testfc1, testes1, testes2, testo1, testo2, testo3],
          TestLabel "Array Comprhenesion" $ TestList [testa1]]



main = runTestTT tests

