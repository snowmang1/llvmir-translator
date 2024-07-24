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
    show EmptyFunction @?= "}\n",
  testCase "Function initiation" $
    show (DefineFunction Integer "sum" [(Integer, L, Name (0, "x")), (Integer, L, Name (0, "y"))] [] EmptyFunction) @?= "define i32 @sum(i32 %x0, i32 %y0) nounwind {\n}\n",
  testCase "Function defined with empty body" $
    show (DefineFunction Integer "sum" [(Integer, L, Name (0, "x")), (Integer, L, Name (0, "y"))] [] (Block (Labled ("Init", [])) EmptyFunction)) @?=
      "define i32 @sum(i32 %x0, i32 %y0) nounwind {\nlabel %Init\n}\n",
  testCase "Function defined with function body" $
    let body      = "%a0 = i32 1\n%b0 = i32 1\n%c0 = add i32 %a0, %b0\nret i32 %c0\n"
        label     = "label %Init\n"
        defTitle  = "define i32 @sum(i32 %a0, i32 %b0) nounwind"
        bind      = [Variable (L, Name (0,"a"), Integer, C "1"), Variable (L, Name (0,"b"), Integer, C "1"),
                     Variable (L, Name (0,"c"), Integer , I (BOps (Add (Integer, L, Name (0, "a")) (Integer, L, Name (0, "b")))))]
        retval    = (Integer, L, Name (0, "c"))
    in
    show (DefineFunction Integer "sum" [(Integer, L, Name(0, "a")),(Integer, L, Name(0, "b"))] [] (Block (Labled ("Init", bind)) (FunctionReturn retval EmptyFunction))) @?=
      defTitle ++ " {\n" ++ label ++ body ++ "}\n"
  ]

bindTesting :: TestTree
bindTesting = testGroup "Binding Testing"
  [
  testCase "simple constant Binding" $
    show (Variable (L, Name (0, "x"), Integer, C "0")) @?= "%x0 = i32 0",
  testCase "Variable add isolation test" $
    show (Variable (L, Name (0, "c"), Integer, I (BOps (Add (Integer, L, Name (0, "a")) (Integer, L, Name (0, "b")))))) @?= "%c0 = add i32 %a0, %b0",
  testCase "testing Instruction BinOps" $
    let outcomes =  [ Add (Integer, L, Name (0, "x")) (Integer, L, Name (1, "x"))        <=> "add i32 %x0, %x1",
                      Add (Integer, L, Name (0, "x")) (Integer, L, Name(0, "y"))         <=> "add i32 %x0, %y0",
                      Add (LLVMDouble, L, Name (0, "x")) (LLVMDouble, L, Name(0, "y"))   <=> "fadd double %x0, %y0",
                      Sub (Long, L, Name (0, "x")) (Long, L, Name(0, "y"))               <=> "sub i64 %x0, %y0",
                      Sub (LLVMFloat, L, Name (0, "x")) (LLVMFloat, L, Name(0, "y"))     <=> "fsub float %x0, %y0",
                      Mul (Integer, L, Name (0, "x")) (Integer, L, Name(0, "y"))         <=> "mul i32 %x0, %y0",
                      Mul (LLVMFloat, L, Name (14, "x")) (LLVMFloat, L, Name(0, "y"))     <=> "fmul float %x14, %y0",
                      Div (LLVMFloat, L, Name (6, "x")) (LLVMFloat, G, Name(0, "y"))     <=> "fdiv float %x6, @y0",
                      Div (Integer, L, Name (0, "x")) (Integer, L, Name(0, "y"))         <=> "div i32 %x0, %y0",
                      Rem (LLVMFloat, L, Name (0, "x")) (LLVMFloat, L, Name(5, "y"))     <=> "frem float %x0, %y5",
                      Rem (Integer, L, Name (0, "x")) (Integer, L, Name(0, "y"))         <=> "srem i32 %x0, %y0"
                    ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s,
  testCase "testing Instruction CallOp" $
    let outcomes =  [ Call Integer "sum" [(Integer, L, Name (0, "x")), (Integer, L, Name (1, "x"))]               <=> "call i32 @sum(i32 %x0, i32 %x1)",
                      Call LLVMFloat   "fsum" [(LLVMFloat, L, Name (0,"x")), (LLVMFloat, L, Name (1,"x"))]      <=> "call float @fsum(float %x0, float %x1)",
                      Call LLVMDouble  "fsum" [(LLVMDouble, L, Name (0,"x")), (LLVMDouble, L, Name (8,"y"))]      <=> "call double @fsum(double %x0, double %y8)"
                    ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s,
  testCase "testing Instruction UnaryOps" $
    show (Neg (LLVMDouble, L, Name (0, "x"))) @?= "fneg double x",
  testCase "testing Instruciton class BitOps" $
    let outcomes =  [ LShift (Integer, L, Name (0, "x")) (Integer, L, Name (0, "y")) <=> "shl i32 %x0, %y0",
                      RShift (Integer, L, Name (0, "x")) (Integer, L, Name (0, "y")) <=> "lshr i32 %x0, %y0",
                      And    (Integer, L, Name (0, "x")) (Integer, L, Name (0, "y")) <=> "and i32 %x0, %y0",
                      Or     (Integer, L, Name (0, "x")) (Integer, L, Name (0, "y")) <=> "or i32 %x0, %y0",
                      XOr    (Integer, L, Name (0, "x")) (Integer, L, Name (0, "y")) <=> "xor i32 %x0, %y0"
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
    let outcomes = [  Name (0,"x") <=> "x0"
                    , Name (1,"x") <=> "x1"
                    , Name (100,"x") <=> "x100"
                    , Name (10,"variable") <=> "variable10" ]
    in 
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s
  ] 
