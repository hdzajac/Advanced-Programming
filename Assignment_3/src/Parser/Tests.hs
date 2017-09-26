module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 


n1 = parseWWS pNumber "123    "
n2 = parseWWS pNumber "   231321123"
n3 = parseWWS pNumber "1"

i1 = parseWWS pIdent "name"
i2 = parseWWS pIdent "  n_321"
i3 = parseWWS pIdent "dsADS_123___"

s1 = parseWWS pString "@DF!#!   "
s2 = parseWWS pString "_!!  dsadeqw"
s3 = parseWWS pString "==}'dsdsa22''222"
s4 = parseWWS pString "true"

-- n4 = pnum "s"

testn1 = TestCase $ assertEqual "Number 1" (Right (Number 123)) (n1)
testn2 = TestCase $ assertEqual "Number 2" (Right (Number 231321123)) (n2)
testn3 = TestCase $ assertEqual "Number 3" (Right (Number 1)) (n3)

testi1 = TestCase $ assertEqual "Ident 1" (Right (Var "name")) (i1)
testi2 = TestCase $ assertEqual "Ident 2" (Right (Var "n_321")) (i2)
testi3 = TestCase $ assertEqual "Ident 3" (Right (Var "dsADS_123___")) (i3)

tests1 = TestCase $ assertEqual "String 1" (Right (String "@DF!#!   ")) (s1)
tests2 = TestCase $ assertEqual "String 2" (Right (String "_!!  dsadeqw")) (s2)
tests3 = TestCase $ assertEqual "String 3" (Right (String "==}'dsdsa22''222")) (s3)
tests4 = TestCase $ assertEqual "String 4" (Right (String "@DF!#!   ")) (s4) -- todo: how to mah against PerseError - impossibru the constructor is private :c


-- testn4 = TestCase $ assertEqual "Number 4" (Right 123) (n1)


tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3],
          TestLabel "Ident" $ TestList [testi1, testi2, testi3],
          TestLabel "String" $ TestList [tests1, tests2, tests3, tests4]]



main = runTestTT tests

