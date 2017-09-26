module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 


n1 = parseWWS pTerminal "123    "
n2 = parseWWS pTerminal "   231321123"
n3 = parseWWS pTerminal "1"

i1 = parseWWS pIdent "name"
i2 = parseWWS pIdent "  n_321"
i3 = parseWWS pIdent "dsADS_123___"

s1 = parseWWS pTerminal "@DF!#!   "
s2 = parseWWS pTerminal "_!!  dsadeqw"
s3 = parseWWS pTerminal "==}'dsdsa22''222"
-- s4 = parseWWS pString "true"

t1 = parseWWS pTerminal "true"
t2 = parseWWS pTerminal "false"
t3 = parseWWS pTerminal "undefined"

e1 = parseWWS pExpr1 "x = 5"


testn1 = TestCase $ assertEqual "Number 1" (Right (Number 123)) (n1)
testn2 = TestCase $ assertEqual "Number 2" (Right (Number 231321123)) (n2)
testn3 = TestCase $ assertEqual "Number 3" (Right (Number 1)) (n3)

testi1 = TestCase $ assertEqual "Ident 1" (Right (Var "name")) (i1)
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


tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3],
          TestLabel "Ident" $ TestList [testi1, testi2, testi3],
          TestLabel "String" $ TestList [tests1, tests2, tests3],
          TestLabel "Terminal" $ TestList [testt1, testt2, testt3],
          TestLabel "Expressions" $ TestList [teste1]]



main = runTestTT tests

