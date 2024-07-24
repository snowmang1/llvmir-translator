module Fxn ( module Fxn ) where

import Types
import Bindings

-- | to define a new function one must first supply a type and the parameters if any exist
-- then we need knowledge of special attributes about the function
-- only then can we define the body
data DefineLLVMFunction = DefineFunction LLVMTypes String [Token] [FxnAttr] LLVMFunctionDef

-- | Helps us define the body of an LLVM function in a structured way utilizing labeled blocks of code
data LLVMFunctionDef = EmptyFunction | Block LabeledBlock LLVMFunctionDef | FunctionReturn Token LLVMFunctionDef

-- | a Block of code is defined as a block of simple bindings for now
-- - FIX: not a comprehensive definition : control flow did not exist at the time of writing
newtype LabeledBlock = Labled (String,[Binding])

-- | to be updated later this is a comprehensive list of function attributes
data FxnAttr = Nounwind

instance Show FxnAttr where
  show Nounwind = "nounwind"

instance Show LabeledBlock where
  show (Labled (lab, body)) = "label " ++ "%" ++ lab ++ "\n" ++ unwrap body
    where
    unwrap :: Show a => [a] -> String
    unwrap []     = ""
    unwrap a      = foldr (\x acc -> show x ++ "\n" ++ acc) "" a

instance Show DefineLLVMFunction where
  show (DefineFunction typ name paramList attrList body) = "define " ++ show typ ++ " @" ++ name ++ "(" ++ tokenUnwrap paramList ++ ") " ++ unwrap attrList
                                                                 ++ " {\n" ++ show body
    where
    tokenUnwrap :: [Token] -> String
    tokenUnwrap []                            = ""
    tokenUnwrap l = let size = length l
                    in
                    foldr (\(tp, s, tn) a -> if length a == size-2 then show tp ++ " " ++ show s ++ show tn ++ a else
                                          show tp ++ " " ++ show s ++ show tn ++ ", " ++ a) "" l
    unwrap :: Show a => [a] -> String
    unwrap [] = "nounwind"
    unwrap a  = foldr (\x acc -> show x ++ ", " ++ acc) "" a

instance Show LLVMFunctionDef where
  show EmptyFunction = "}\n"
  show (Block lb fd) = show lb ++ show fd
  show (FunctionReturn (t, s, n) fd) = "ret " ++ show t ++ " " ++ show s ++ show n ++ "\n" ++ show fd
