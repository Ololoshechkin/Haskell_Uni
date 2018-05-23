module InterpreterTest where
import           Interpreter

import qualified Data.ByteString.UTF8 as S8
import qualified Data.Map.Strict      as Map
import           Test.Hspec
import           Test.Tasty           (TestTree)
import           Test.Tasty.Hspec     (Spec, describe, it, shouldBe, testSpec)

testEval :: IO TestTree
testEval = testSpec "All tests" spec

spec :: Spec
spec = do
    let map = Map.singleton "x" 1
    describe "Evaluation Expression Bool Tests" $ do
        it "EqTest" $ do
            runEvalBool (Eq (Const 1) (Const 2)) map `shouldReturn` False
            runEvalBool (Eq (Const 2) (Const 2)) map `shouldReturn` True
        it "NEqTest" $ do
            runEvalBool (NEq (Const 1) (Const 2)) map `shouldReturn` True
            runEvalBool (NEq (Const 2) (Const 2)) map `shouldReturn` False
        it "GtTest" $ do
            runEvalBool (Gt (Const 1) (Const 2)) map `shouldReturn` False
            runEvalBool (Gt (Const 2) (Const 1)) map `shouldReturn` True
        it "GtEqTest" $
            runEvalBool (GtEq (Const 2) (Const 2)) map `shouldReturn` True
        it "LtTest" $ do
            runEvalBool (Lt (Const 1) (Const 2)) map `shouldReturn` True
            runEvalBool (Lt (Const 2) (Const 1)) map `shouldReturn` False
        it "LtEqTest" $
            runEvalBool (LtEq (Const 2) (Const 2)) map `shouldReturn` True
    describe "Evaluation Expression Tests" $ do
        it "ConstTest" $
            runEval (Const 1) map `shouldReturn` 1
        it "VarTest" $ do
            runEval (Var "x") map `shouldReturn` 1
            runEval (Var "y") map `shouldThrow` (== UndefinedVariable "y")
        it "AddTest" $ do
            runEval (Add (Var "x") (Const 1)) map `shouldReturn` 2
            runEval (Add (Const 10) (Const 1)) map `shouldReturn` 11
        it "SubTest" $ do
            runEval (Sub (Const 10) (Const 1)) map `shouldReturn` 9
            runEval (Sub (Var "x") (Const 1)) map `shouldReturn` 0
        it "MulTest" $ do
            runEval (Mul (Const 10) (Const 2)) map `shouldReturn` 20
            runEval (Mul (Var "x") (Const 3)) map `shouldReturn` 3
        it "DivTest" $ do
            runEval (Div (Const 10) (Const 2)) map `shouldReturn` 5
            runEval (Div (Var "x") (Const 1)) map `shouldReturn` 1
            runEval (Div (Var "x") (Const 0)) map `shouldThrow` (== DivisionByZero)
        it "LetTest" $ do
            runEval (Let "x" (Const 2) $ Var "x") map `shouldReturn` 2
            runEval
                (Let "y" (Add (Const 2) (Const 2))
                     (Add (Var "y") (Mul (Const 3) ("x" `Let` Const 2 $ Var "x"))))
                map `shouldReturn`10
    describe "Statement Tests" $
        it "runStatement" $ do
            runStatement
                [Assignment {var = "x", val = Add {a = Const 4, b = Mul {a = Const 2, b = Const 3}}}]
                `shouldReturn` Map.singleton "x" 10
            runStatement
                [Assignment {var = "x", val = Add {a = Const 4, b = Mul {a = Const 2, b = Const 3}}},
                Reassignment {var = "x", val = Const 5}]
                `shouldReturn` Map.singleton "x" 5
            runStatement [OutState {val = Add {a = Var "x", b = Const 7}}]
                `shouldThrow` (== ComputationException
                    OutState {val = Add {a = Var "x", b = Const 7}}
                    (UndefinedVariable "x"))
    describe "Parser Tests" $ do
        it "base" $ do
            parseExpressions "2 + 10" `shouldReturn` Add (Const (2::Int)) (Const 10)
            parseWithExpressionEvaluation "2 + 10" `shouldReturn` 12
        it "parseExpressions" $ do
            parseExpressions " 2 * 4" `shouldReturn` Mul (Const 2) (Const 4)
            parseExpressions "( 3 + 2 * 4 - (6/2))" `shouldReturn`
                Sub {a = Add {a = Const 3, b = Mul {a = Const 2, b = Const 4}}, b = Div {a = Const 6, b = Const 2}}
        it "parseWithExpressionEvaluation" $ do
            parseWithExpressionEvaluation " 2 * 4" `shouldReturn` 8
            parseWithExpressionEvaluation "( 3 + 2 * 4 - (6/2))" `shouldReturn` 8
            parseWithExpressionEvaluation "let x = 2 in x" `shouldReturn` 2
            parseWithExpressionEvaluation "(let x = 2 in x)" `shouldReturn` 2
            parseWithExpressionEvaluation " (   let x = 2 in x )   "  `shouldReturn` 2
            parseWithExpressionEvaluation "   11 + 4 * ( let x   =2 in  x) - 7" `shouldReturn` 12
        it "parseStatement" $ do
            parseStatement "mut x = 4 + 2 * 3" `shouldReturn`
                Assignment {var = "x", val = Add {a = Const 4, b = Mul {a = Const 2, b = Const 3}}}
            parseStatement "x = 5" `shouldReturn` Reassignment {var = "x", val = Const 5}
            parseStatement "> x" `shouldReturn` InState {var = "x"}
            parseStatement "< x + 7" `shouldReturn` OutState {val = Add {a = Var "x", b = Const 7}}
        it "parseWithStatementEvaluation" $ do
            parseWithStatementEvaluation "mut x = 4 + 3 * 2" `shouldReturn` Map.singleton "x" 10
            parseWithStatementEvaluation "mut x = 4 + 3 * 2 + y" `shouldThrow` (== ComputationException
                Assignment {var = "x", val = Add {a = Add {a = Const 4, b = Mul {a = Const 3, b = Const 2}}, b = Var "y"}}
                (UndefinedVariable "y"))
            parseWithStatementEvaluation "x = 4 + 3 * 2" `shouldThrow` (== UndefinedVariableException "x")
        let testProgram = S8.fromString $ unlines ["mut x = 1 + 4","x = x * 5 / (let z = 5 in z) + (let x = 3 in x)", "< x + 1"]
        let testForLoop = S8.fromString $ unlines ["mut y = 1", "for 0 to 2 { y = y +  1\n}"]
        let testForLoopRed = S8.fromString $ unlines ["mut y = 1", "for 0 to 2 { mut x = y + 1\n}"]
        let testIfTrue = S8.fromString $ unlines ["mut y = 1", "if true then {y = 2\n} else {y = 3\n}"]
        let testIfFalse = S8.fromString $ unlines ["mut y = 1", "if false then {y = 2\n} else {y = 3\n}"]
        let testIfEqTrue = S8.fromString $ unlines ["if 1 == (2 - 1 )then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfNEqTrue = S8.fromString $ unlines ["if 1 != (2  + 4 )then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfEqFalse = S8.fromString $ unlines ["if (1 * 2) == (2 - 1 )then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfNEqFalse = S8.fromString $ unlines ["if (1 * 2) != (2 - 1 + 1)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfGtTrue = S8.fromString $ unlines ["if (1 * 2) > (2 - 1 )then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfGtFalse = S8.fromString $ unlines ["if (1 * 2) > (2 - 1  + 7)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtTrue = S8.fromString $ unlines ["if (1 * 2) < (2 + 1 * 4)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtFalse = S8.fromString $ unlines ["if (1 * 2) < (2 - 1  + 7 - 7)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfGtEqTrue = S8.fromString $ unlines ["if (1 * 2) >= (2 - 1)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfGtEqTrue_ = S8.fromString $ unlines ["if (1 * 2) >= 2 then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfGtEqFalse = S8.fromString $ unlines ["if (1 * 2) >= (2 - 1 + 8)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtEqTrue = S8.fromString $ unlines ["if 1 <= (2 - 1)then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtEqTrue_ = S8.fromString $ unlines ["if (1 * 2) <= (2 + 4) then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtEqFalse = S8.fromString $ unlines ["if (1 * 2) <= (2 - 1) then {mut y = 2\n} else {mut y = 3\n}"]
        let testIfLtEqVarTrue = S8.fromString $ unlines ["mut y = 1","if (y * 2) <= 2 then {y = 2\n} else {y = 3\n}"]
        it "runProgram" $ do
            runProgram_ "testCombo" testProgram `shouldReturn` Map.singleton "x" 8
            runProgram_ "testFor" testForLoop `shouldReturn` Map.singleton "y" 3
            runProgram_ "testForRed" testForLoopRed `shouldThrow` (==  ValReassignmentException "x")
            runProgram_ "testIfTrue" testIfTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfFalse" testIfFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfExprTrue" testIfEqTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfExprFalse" testIfEqFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfNEqTrue" testIfNEqTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfNEqFalse" testIfNEqFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfGtTrue" testIfGtTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfGtFalse" testIfGtFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfLtTrue" testIfLtTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfLtFalse" testIfLtFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfGtEqTrue" testIfGtEqTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfGtEqTrue_" testIfGtEqTrue_ `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfGtEqFalse" testIfGtEqFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfLtEqTrue" testIfLtEqTrue `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfLtEqTrue_" testIfLtEqTrue_ `shouldReturn` Map.singleton "y" 2
            runProgram_ "testIfLtEqFalse" testIfLtEqFalse `shouldReturn` Map.singleton "y" 3
            runProgram_ "testIfLtEqVarTrue" testIfLtEqVarTrue `shouldReturn` Map.singleton "y" 2


