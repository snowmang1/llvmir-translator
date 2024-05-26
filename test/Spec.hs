import Test.Tasty
import Test.Tasty.HUnit

main :: IO()
main = defaultMain testing

testing :: TestTree
testing = testGroup "All Tests"
  [
  testCase "smoke" $
    True @?= True
  ]
