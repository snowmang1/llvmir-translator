import Test.Tasty
import Test.Tasty.HUnit

import Lib
import Types

main :: IO()
main = defaultMain allTests

allTests :: TestTree
allTests =  testGroup "Tests" [typeTesting, bindTesting]


(<=>) :: Show a => a -> String -> Maybe String
(<=>) a b = if show a == b then Nothing else Just ("Failure: " ++ show a ++ " =/= " ++ show b)

bindTesting :: TestTree
bindTesting = testGroup "Binding Testing"
  [
    testCase "simple constant Binding" $
      show (ConstVar (L, Lexeme 0 "x", Integer, "0")) @?= "%x0 = i32 0"
  ]

typeTesting :: TestTree
typeTesting = testGroup "Type Testing"
  [
  testCase "smoke" $
    True @?= True,
  testCase "Type Number Integer is 32 bits" $
    show Integer @?= "i32",
  testCase "Type Number Float is a float" $
    show Float @?= "float",
  testCase "Type Number Double is a double" $
    show Double @?= "double",
  testCase "Type Number Long is 64 bits" $
    show Long @?= "i64",
  testCase "Type List of length 5" $
    let outcomes = [Array Integer 5 <=> "[5 x i32]",
                    Array Long 5 <=> "[5 x i64]",
                    Array Float 5 <=> "[5 x float]",
                    Array Double 5 <=> "[5 x double]",
                    Array Character 5 <=> "[5 x i8]",
                    Array Boolean 5 <=> "[5 x i1]" ]
        searchOutcomes = foldr f Nothing
          where
            f Nothing (Just s) = Just s
            f (Just s) Nothing   = Just s
            f Nothing Nothing  = Nothing
            f (Just _) (Just s2) = Just s2
    in
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s,
  testCase "Name mangling tests" $
    let outcomes = [  Lexeme 0 "x" <=> "x0"
                    , Lexeme 1 "x" <=> "x1"
                    , Lexeme 100 "x" <=> "x100"
                    , Lexeme 10 "variable" <=> "variable10" ]
        searchOutcomes = foldr f Nothing
          where
            f (Nothing) (Just s) = Just s
            f (Just s) Nothing   = Just s
            f Nothing (Nothing)  = Nothing
            f (Just _) (Just s2) = Just s2
    in
    case searchOutcomes outcomes of
      Nothing -> assertBool "" True
      Just s  -> assertFailure s
  ] 
