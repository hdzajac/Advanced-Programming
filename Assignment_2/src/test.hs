import Test.HUnit
import SubsInterpreter
import SubsAst

---------------------------------------------------

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
general5 = Compr (ACIf (Call "<" [ (Number 1), (Number 2) ]) (ACBody (Comma (Assign "x" (Number 3) ) (Var "x"))))
general6 = Comma (Comma (Assign "x" (Number 10)) (Assign "y" (Number 11))) 
               (Comma (Compr (ACIf (Call "===" [ (Var "x"), (Var "y" ) ]) (ACBody (Comma (Assign "x" (Number 3) ) (Assign "y" (Number 3)))))) (Array [(Var "x"),( Var "y")]))
general7 = Comma (Comma (Assign "x" (Number 11)) (Assign "y" (Number 11))) 
               (Comma (Compr (ACIf (Call "===" [ (Var "x"), (Var "y" ) ]) (ACBody (Comma (Assign "x" (Number 3) ) (Assign "y" (Number 3)))))) (Array [(Var "x"),( Var "y")]))
general8 = Comma (Comma (Assign "x" (Number 11)) (Assign "y" (Number 11))) 
               (Comma (Compr (ACIf (Call "===" [(Var "x"), (Var "y" ) ]) (ACBody  (Assign "x" (Call "*" [(Var "x"),(Number 3)]))))) (Array [(Var "x"),( Var "y")]))


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
testgeneral5 = TestCase $ assertEqual "General 5" (Right (IntVal 3)) (runExpr general5)
testgeneral6 = TestCase $ assertEqual "General 6 Scoping" (Right (ArrayVal [IntVal 10,IntVal 11])) (runExpr general6)
testgeneral7 = TestCase $ assertEqual "General 7 Scoping" (Right (ArrayVal [IntVal 3,IntVal 3])) (runExpr general7)
testgeneral8 = TestCase $ assertEqual "General 8 Scoping" (Right (ArrayVal [IntVal 33,IntVal 11])) (runExpr general8)

---- Array Compr Test
aCompr1 = Comma (Comma (Assign "xs" (Array [Number 0, Number 1, Number 2, Number 3,Number 4,Number 5, Number 6, Number 7, Number 8, Number 9]))
          (Comma (Assign "squares" (Compr (ACFor "x" (Var "xs")(ACBody (Call "*" [Var "x",Var "x"])))))
		    (Comma (Assign "evens" (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x", Number 2], Number 0])
              (ACBody (Var "x")))))) (Assign "many_a" (Compr (ACFor "x" (Var "xs") (ACFor "y" (Var "xs") (ACBody (String "a"))))))))) 
                  (Array [Var "xs", Var "squares", Var "evens", Var "many_a"])
aComprTest1 = TestCase $ assertEqual "Array compr 1"  (Right (ArrayVal [ArrayVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9],
                 ArrayVal [IntVal 0,IntVal 1,IntVal 4,IntVal 9,IntVal 16,IntVal 25,IntVal 36,IntVal 49,IntVal 64,IntVal 81],
                   ArrayVal [IntVal 0,IntVal 2,IntVal 4,IntVal 6,IntVal 8],ArrayVal [StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                     StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                       StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                        StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                         StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                          StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                            StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                             StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                              StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                               StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a"]])) (runExpr aCompr1)
							   
aCompr2 = Compr (ACFor "i" (Array [Number 0]) (ACBody (Var "i")))  
aComprTest2 =  TestCase $ assertEqual "Array compr 2 "  (Right (ArrayVal [IntVal 0])) (runExpr aCompr2)

aCompr3 = Compr (ACFor "i" (Array [Number 0]) (ACBody (Assign "i" (Call "+" [Var "i", Number 1])))) 
aComprTest3 =  TestCase $ assertEqual "Array compr 3 "  (Right (ArrayVal [IntVal 1])) (runExpr aCompr3)

aCompr4 = Comma (Assign "x" (Number 42)) (Comma (Assign "y" (Compr (ACFor "x" (String "abc")(ACBody (Var "x"))))) (Array [Var "x", Var "y"]))
aComprTest4 =  TestCase $ assertEqual "Array compr 4 "  (Right (ArrayVal [IntVal 42,ArrayVal [StringVal "a",StringVal "b",StringVal "c",StringVal ""]])) (runExpr aCompr4)

aCompr5 = Assign "xs" (Array [Number 0, Number 1, Number 2, Number 3, Number 4,Number 5, Number 6, Number 7, Number 8, Number 9])
aComprTest5 =  TestCase $ assertEqual "Array compr 5 "  (Right (ArrayVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9])) (runExpr aCompr5)

aCompr6 = Comma (Assign "xs" (Array [Number 1, Number 2, Number 3, Number 4])) (Compr (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x", Var "x"]))))
aComprTest6 =  TestCase $ assertEqual "Array compr 6 "  (Right (ArrayVal [IntVal 1,IntVal 4,IntVal 9,IntVal 16])) (runExpr aCompr6)


aCompr7 = Comma (Assign "xs" (Array [Number 0, Number 1, Number 2, Number 3, Number 4,Number 5, Number 6, Number 7, Number 8, Number 9])) (Compr (ACFor "x" ( Var "xs") (ACIf (Call "===" [Call "%" [Var "x", Number 2], Number 0])(ACBody (Var "x")))))
aComprTest7 =  TestCase $ assertEqual "Array compr 7 "  (Right (ArrayVal [IntVal 0,IntVal 2,IntVal 4,IntVal 6,IntVal 8])) (runExpr aCompr7)



tests = TestList [TestLabel "Equality" $ TestList [testET, testEF],
                  TestLabel "Inequality - smaller" $ TestList [testLT, testLF],
                  TestLabel "Sum" $ TestList [testST, testSF],
                  TestLabel "Minus" $ TestList [testMT, testMF],
                  TestLabel "Modulo " $ TestList [testMODT, testMODF, testMODE],
                  TestLabel "Assignment" $ TestList [testAssignT, testgeneral1, testgeneral2, testgeneral3, testgeneral4,testgeneral5,testgeneral6,testgeneral7,testgeneral8],
				  TestLabel "Array Compr" $ TestList [aComprTest1, aComprTest2, aComprTest3, aComprTest4, aComprTest5,aComprTest6,aComprTest7]]






main = runTestTT tests

