module Parser.Tests where

import Parser.Impl
import SubsAst

import Test.HUnit
import Text.Parsec 


n1 = parse int "error message 1" "123"
n2 = parse int "error message 2" "231321123"
n3 = parse int "error message 3" "1"
-- n4 = pnum "s"

testn1 = TestCase $ assertEqual "Number 1" (Right (Number 123)) (n1)
testn2 = TestCase $ assertEqual "Number 2" (Right (Number 231321123)) (n2)
testn3 = TestCase $ assertEqual "Number 3" (Right (Number 1)) (n3)
-- testn4 = TestCase $ assertEqual "Number 4" (Right 123) (n1)


tests = TestList [TestLabel "Numbers" $ TestList [testn1, testn2, testn3]]



main = runTestTT tests

