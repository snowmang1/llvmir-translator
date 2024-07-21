import Test.Tasty
import Test.Tasty.HUnit

import Bindings
import Types
import Fxn

main :: IO()
main = defaultMain allTests

allTests :: TestTree
allTests =  testGroup "Tests" [typeTesting, bindTesting, functionDefTesting]

searchOutcomes :: [Maybe String] -> Maybe String
searchOutcomes = foldr f Nothing
  where
    f :: Maybe String -> Maybe String -> Maybe String
    f Nothing (Just s)   = Just s
    f (Just s) Nothing   = Just s
    f Nothing Nothing    = Nothing
    f (Just _) (Just s2) = Just s2

(<=>) :: Show a => a -> String -> Maybe String
(<=>) a b = if show a == b then Nothing else Just ("Failure: " ++ show a ++ " =/= " ++ show b)

functionDefTesting :: TestTree
functionDefTesting = testGroup "Fxn Testing"
  [
  testCase "simple Empty Function print" $
    show EmptyFunction @?= "}\n"
  ]

bindTesting :: TestTree
bindTesting = testGroup "Binding Testing"
  [
  testCase "simple constant Binding" $
    show (Variable (L, Lexeme 0 "x", Integer, "0")) @?= "%x0 = i32 0",
  testCase "testing Instruction BinOps" $
    let outcomes =  [ Add (Integer, "x0") (Integer, "x1")       <=> "add i32 x0, x1",
                      Add (Integer, "x") (Integer, "y")         <=> "add i32 x, y",
                      Add (LLVMDouble, "x") (LLVMDouble, "y")   <=> "fadd double x, y",
                      Sub (Long, "x") (Long, "y")               <=> "sub i64 x, y",
                      Sub (LLVMFloat, "x") (LLVMFloat, "y")     <=> "fsub float x, y",
                      Mul (Integer, "x") (Integer, "y")         <=> "mul i32 x, y",
                      Mul (LLVMFloat, "x") (LLVMFloat, "y")     <=> "fmul float x, y",
                      Div (LLVMFloat, "x") (LLVMFloat, "y")     <=> "fdiv float x, y",
                      Div (Integer, "x") (Integer, "y")         <=> "div i32 x, y",
                      Rem (LLVMFloat, "x") (LLVMFloat, "y")     <=> "frem float x, y",
                      Rem (Integer, "x") (Integer, "y")         <=> "srem i32 x, y"
                    ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s,
  testCase "testing Instruction CallOp" $
    let outcomes =  [ Call Integer "sum" [(Integer, "x0"), (Integer, "x1")]               <=> "call i32 @sum(i32 x0, i32 x1)",
                      Call LLVMFloat   "fsum" [(LLVMFloat, "x0"), (LLVMFloat, "x1")]      <=> "call float @fsum(float x0, float x1)",
                      Call LLVMDouble  "fsum" [(LLVMDouble, "x"), (LLVMDouble, "y")]      <=> "call double @fsum(double x, double y)"
                    ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s
  ]

typeTesting :: TestTree
typeTesting = testGroup "Type Testing"
  [
  testCase "smoke" $
    True @?= True,
  testCase "Type Number Integer is 32 bits" $
    show Integer @?= "i32",
  testCase "Type Number LLVMFloat is a float" $
    show LLVMFloat @?= "float",
  testCase "Type Number LLVMDouble is a double" $
    show LLVMDouble @?= "double",
  testCase "Type Number Long is 64 bits" $
    show Long @?= "i64",
  testCase "Type List of length 5" $
    let outcomes = [Array Integer 5 <=> "[5 x i32]",
                    Array Long 5 <=> "[5 x i64]",
                    Array LLVMFloat 5 <=> "[5 x float]",
                    Array LLVMDouble 5 <=> "[5 x double]",
                    Array Character 5 <=> "[5 x i8]",
                    Array Boolean 5 <=> "[5 x i1]" ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s,
  testCase "Name mangling tests" $
    let outcomes = [  Lexeme 0 "x" <=> "x0"
                    , Lexeme 1 "x" <=> "x1"
                    , Lexeme 100 "x" <=> "x100"
                    , Lexeme 10 "variable" <=> "variable10" ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s
  ] 
