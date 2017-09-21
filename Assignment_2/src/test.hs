import Test.HUnit
import SubsInterpreter
import SubsAst

ef = Call "===" [Number 2, Number 1]
et = Call "===" [Number 2, Number 2]

lt = Call "<" [Number 2, Number 10]
lf = Call "<" [Number 20, Number 2]

st = Call "+" [Number 2, Number 10]
sf = Call "+" [Number 20, TrueConst]

mt = Call "-" [Number 20, Number 10]
mf = Call "-" [Number 20, TrueConst]

modt = Call "%" [Number 5, Number 2 ]
modf = Call "%" [Number 20, Number 0]
mode = Call "%" [Number 20, TrueConst]

assignt = Assign "x" (Number 1)

general1 = Comma (Assign "x" (Number 1)) (Call "+" [(Var "x"),(Number 3)])
general2 = Compr (ACIf (Call "===" [ (Number 1), (Number 1) ]) (ACBody (Number 2) ))
general3 = Call "===" [Array [Number 2, Number 2], Array [Number 2, Number 2]]
general4 = Comma (Assign "x" (Number 0)) (Comma (Compr (ACFor "y" (Array [Number 1,Number 2,Number 3]) (ACBody (Assign "x" (Var "y"))))) (Var "x"))


testEF = TestCase $ assertEqual "Inequality" (Right FalseVal) (runExpr ef)
testET = TestCase $ assertEqual "Equality" (Right TrueVal) (runExpr et)

testLT = TestCase $ assertEqual "Greater" (Right FalseVal) (runExpr lf)
testLF = TestCase $ assertEqual "Smaller" (Right TrueVal) (runExpr lt)

testST = TestCase $ assertEqual "Correct sum" (Right (IntVal 12)) (runExpr st)
testSF = TestCase $ assertEqual "Added True" (Left "\"+\" applied to incompatible types, try any combination of String and Int") (runExpr sf)

testMT = TestCase $ assertEqual "Correct subtraction" (Right (IntVal 10)) (runExpr mt)
testMF = TestCase $ assertEqual "Subtracted True" (Left "\"-\" applied to incompatible types, try: Int - Int") (runExpr mf)

testMODT = TestCase $ assertEqual "Correct modulo" (Right (IntVal 1)) (runExpr modt)
testMODF = TestCase $ assertEqual "Modulo 0" (Left "\"% 0\" is undefined, try different value") (runExpr modf)
testMODE = TestCase $ assertEqual "Modulo true" (Left "\"*\" applied to incompatible types, try: Int % Int") (runExpr mode)

testAssignT = TestCase $ assertEqual "Correct assignment" (Right (IntVal 1)) (runExpr assignt)
testgeneral1 = TestCase $ assertEqual "General 1" (Right (IntVal 4)) (runExpr general1)
testgeneral2 = TestCase $ assertEqual "General 2" (Right (IntVal 2)) (runExpr general2)
testgeneral3 = TestCase $ assertEqual "General 3" (Right TrueVal) (runExpr general3)
testgeneral4 = TestCase $ assertEqual "General 4" (Right (IntVal 3)) (runExpr general4)



tests = TestList [TestLabel "Equality" $ TestList [testET, testEF],
                  TestLabel "Inequality - smaller" $ TestList [testLT, testLF],
                  TestLabel "Sum" $ TestList [testST, testSF],
                  TestLabel "Minus" $ TestList [testMT, testMF],
                  TestLabel "Modulo " $ TestList [testMODT, testMODF, testMODE],
                  TestLabel "Assignment" $ TestList [testAssignT, testgeneral1, testgeneral2, testgeneral3, testgeneral4]]
main = runTestTT tests

