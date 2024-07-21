module Fxn ( module Fxn ) where

import Types
import Bindings

-- | an LLVM IR function definition can be naively expressed as a collection of labeled blocks of simple IR code
-- such that any piece of the function can be expressed as
data LLVMFunctionDef = EmptyFunction | InitialBlock LLVMTypes String [Token] [FxnAttr] | Block LabeledBlock LLVMFunctionDef

-- | a Block of code is defined as a block of simple bindings for now
-- - FIX: not a comprehensive definition
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
    unwrap (h:[]) = show h ++ "\n"
    unwrap (h:t)  = show h ++ "\n" ++ unwrap t

instance Show LLVMFunctionDef where
  show (InitialBlock typ name paramList attrList) = "define " ++ show typ ++ " @" ++ name ++ "(" ++ tokenUnwrap paramList ++ ") " ++ unwrap attrList ++ "{\n"
    where
    tokenUnwrap :: [Token] -> String
    tokenUnwrap []                            = ""
    tokenUnwrap ((token_type, token_name):[]) = show token_type ++ " " ++ token_name
    tokenUnwrap ((token_type, token_name):t)  = show token_type ++ " " ++ token_name ++ ", " ++ tokenUnwrap t
    unwrap :: Show a => [a] -> String
    unwrap []      = ""
    unwrap (h:[])  = show h
    unwrap (h:t)   = show h ++ ", " ++ unwrap t
  show EmptyFunction = "}\n"
  show (Block lb fd) = show lb ++ "\n" ++ show fd
