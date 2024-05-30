import Test.Tasty
import Test.Tasty.HUnit

import Lib
import Functions

main :: IO()
main = defaultMain testing

testing :: TestTree
testing = testGroup "All Tests"
  [
  testCase "smoke" $
    True @?= True,
  testCase "primitive Binding print" $
    show (Bind (Local "x", Number, "5")) @?= "%x = i32 5\n",
  testCase "primitive Addition print" $
    show (Binary (Local "x", Number, Add, "x0", "5")) @?= "%x = add i32 x0, 5\n",
  testCase "primitive Subtraction print" $
    show (Binary (Local "x", Number, Sub, "x0", "1")) @?= "%x = sub i32 x0, 1\n",
  testCase "function begin translation" $
    let paramlist = Full [(Number, Global "x"), (Number, Local "y"), (Number, Local "z")] in
    show (Begin (Number, Global "sum", paramlist)) @?= "define i32 @sum(i32 @x, i32 %y, i32 %z) {\n",
  testCase "function begin and end translation" $
    let paramlist = Full [(Number, Global "x"), (Number, Local "y"), (Number, Local "z")] in
    (show (Begin (Number, Global "sum", paramlist)) ++ show EndFunction)
      @?= "define i32 @sum(i32 @x, i32 %y, i32 %z) {\n}\n"

  ]
