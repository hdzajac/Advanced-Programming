module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 



----------------------------------------------------------
------------------- S E T 1 ------------------------------

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
s3 = parseWWS pString  "'==}\\'dsdsa22\\'\\'222'"
s4 = parseWWS pString  "' '"
s5 = parseWWS pString  "'\\t \\\' \\\'\\n\'"
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
o2 = parseWWS pStart "a + b + b - 3"
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
tests5 = TestCase $ assertEqual "String 5" (Right (String "\t ' '\n")) (s5)
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


-------------------------------------------------------------------
----------        S E T 2               ---------------------------

errorWrapper::  Either ParseError a -> [Char]
errorWrapper p = case p of Right _ -> "No Error"
                           Left _ -> "Error"

negativeNo1 = errorWrapper (parseWWS pStart  "7A   ")
negativeNo2 = errorWrapper (parseWWS pStart  "7 a   ")
negativeNo3 = errorWrapper (parseWWS pStart  " 77 a   ")

negativeFunc1 = errorWrapper (parseWWS pStart  "5 = x")
negativeFunc2 = errorWrapper (parseWWS pStart  "f (1,2 ")
negativeFunc3 = errorWrapper (parseWWS pStart  "f 1,2 )")

negativeString1 = errorWrapper (parseWWS pString "' sas ")
negativeString2 = errorWrapper (parseWWS pString "StringWithoutQuotes")
negativeString3 = errorWrapper (parseWWS pStart "'bad esc\ape \t  '")
negativeString4 = errorWrapper (parseWWS pStart "'escaped end quote \t \\\'")
negativeString5 = errorWrapper (parseWWS pStart "'hidden\ttab'")
negativeString6 = parseWWS pStart "'new bbb\\\naa'"


interpTest1 = parseWWS pStart  "true"
interpTest2 = parseWWS pStart  "true1"


nTestn1 = TestCase $ assertEqual "Negative No 1" ("Error") (negativeNo1)
nTestn2 = TestCase $ assertEqual "Negative No 2" ("Error") (negativeNo2)
nTestn3 = TestCase $ assertEqual "Negative No 3" ("Error") (negativeNo3)
nTestn4 = TestCase $ assertEqual "Interp Test 1" (Right (TrueConst)) (interpTest1)
nTestn5 = TestCase $ assertEqual "Interp Test 2" (Right (Var "true1")) (interpTest2)
nTestn6 = TestCase $ assertEqual "Negative Func 1" ("Error") (negativeFunc1)
nTestn7 = TestCase $ assertEqual "Negative Func 2" ("Error") (negativeFunc2)
nTestn8 = TestCase $ assertEqual "Negative Func 3" ("Error") (negativeFunc3)

precedenceTest1 = parseWWS pStart "2+3*2"
precedenceTest2 = parseWWS pStart "1*4+3*2"
precedenceTest3 = parseWWS pStart "(1+2)+3*2"
precedenceTest4 = parseWWS pStart "(1+2)+3+2"
precedenceTest5 = parseWWS pStart  "(1+(2+3)+2+(2-3-(3-1)))"
precedenceTest6 = parseWWS pStart  "1%2-1"
precedenceTest7 = parseWWS pStart  "1-1%2-1"
precedenceTest8 = parseWWS pStart  "1-2%(2-1)"
precedenceTest9 = parseWWS pStart  "[1,2,3,[1,3]]"
precedenceTest10 = parseWWS pStart  "[ for (x of xs) x * x ]"
precedenceTest11 = parseWWS pStart  "[ for (x of xs) x * x + 1 ]"
precedenceTest12 = parseWWS pStart  "[ for (x of xs) x * (x + 1) ]"
precedenceTest13 = parseWWS pStart  "[ for (x of xs) if (x % 2 === 0) x ]"
precedenceTest14 = parseWWS pStart  "[ for (x of xs) if (x % (2 - 1) === 0) x ]"

pTest1 = TestCase $ assertEqual "Precedence test 1" (Right (Call "+" [Number 2,Call "*" [Number 3,Number 2]])) (precedenceTest1)
pTest2 = TestCase $ assertEqual "Precedence test 2" (Right (Call "+" [Call "*" [Number 1,Number 4],Call "*" [Number 3,Number 2]])) (precedenceTest2)
pTest3 = TestCase $ assertEqual "Precedence test 3" (Right (Call "+" [Call "+" [Number 1,Number 2],Call "*" [Number 3,Number 2]])) (precedenceTest3)
pTest4 = TestCase $ assertEqual "Precedence test 4" (Right (Call "+" [Call "+" [Call "+" [Number 1,Number 2],Number 3],Number 2])) (precedenceTest4)
pTest5 = TestCase $ assertEqual "Precedence test 5" (Right (Call "+" [Call "+" [Call "+" [Number 1,Call "+" [Number 2,Number 3]],Number 2],Call "-" [Call "-" [Number 2,Number 3],Call "-" [Number 3,Number 1]]])) (precedenceTest5)
pTest6 = TestCase $ assertEqual "Precedence test 6" (Right (Call "-" [Call "%" [Number 1,Number 2],Number 1])) (precedenceTest6)
pTest7 = TestCase $ assertEqual "Precedence test 7" (Right (Call "-" [Call "-" [Number 1,Call "%" [Number 1,Number 2]],Number 1])) (precedenceTest7)
pTest8 = TestCase $ assertEqual "Precedence test 8" (Right (Call "-" [Number 1,Call "%" [Number 2,Call "-" [Number 2,Number 1]]])) (precedenceTest8)
pTest9 = TestCase $ assertEqual "Precedence test 9" (Right (Array [Number 1,Number 2,Number 3,Array [Number 1,Number 3]])) (precedenceTest9)
pTest10 = TestCase $ assertEqual "Precedence test 10" (Right (Compr (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x",Var "x"]))))) (precedenceTest10)
pTest11 = TestCase $ assertEqual "Precedence test 11" (Right (Compr (ACFor "x" (Var "xs") (ACBody (Call "+" [Call "*" [Var "x",Var "x"],Number 1]))))) (precedenceTest11)
pTest12 = TestCase $ assertEqual "Precedence test 12" (Right (Compr (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x",Call "+" [Var "x",Number 1]]))))) (precedenceTest12)
pTest13 = TestCase $ assertEqual "Precedence test 12" (Right (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x",Number 2],Number 0]) (ACBody (Var "x")))))) (precedenceTest13)
pTest14 = TestCase $ assertEqual "Precedence test 13" (Right (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x",Call "-" [Number 2,Number 1]],Number 0]) (ACBody (Var "x")))))) (precedenceTest14)



nStringTest1 = TestCase $ assertEqual "Negative String 1" ("Error") (negativeString1)
nStringTest2 = TestCase $ assertEqual "Negative String 2" ("Error") (negativeString2)
nStringTest3 = TestCase $ assertEqual "Negative String 3" ("Error") (negativeString3)
nStringTest4 = TestCase $ assertEqual "Negative String 4" ("Error") (negativeString4)
nStringTest5 = TestCase $ assertEqual "Negative String 5" ("Error") (negativeString5)
nStringTest6 = TestCase $ assertEqual "String 6" (Right (String "new bbbaa")) negativeString6


tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3, testn4, testn5, testn6],
                  TestLabel "Ident" $ TestList [testi1, testi2, testi3],
                  TestLabel "String" $ TestList [tests1, tests2, tests3, tests4, tests5, tests6],
                  TestLabel "Terminal" $ TestList [testt1, testt2, testt3],
                  TestLabel "Expressions" $ TestList [teste1, teste2, teste3, testfc1, testes1, testes2, testo1, testo2, testo3],
                  TestLabel "Array Comprhenesion" $ TestList [testa1],
                  TestLabel "Negative Numbers" $ TestList [nTestn1,nTestn2,nTestn3],
                  TestLabel "Interpretation test" $ TestList [nTestn4,nTestn5],
                  TestLabel "Negative Functions Test" $ TestList [nTestn6,nTestn7,nTestn8],
                  TestLabel "Precedence tests" $ TestList [pTest1,pTest2,pTest3,pTest4,pTest5,pTest6,pTest7,pTest8,pTest9,pTest10,pTest11,pTest12,pTest13,pTest14],
                  TestLabel "Negative String tests" $ TestList [nStringTest1,nStringTest2,nStringTest3,nStringTest4,nStringTest5,nStringTest6]]




main = runTestTT tests