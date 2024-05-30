module Lib ( module Lib ) where

import Functions

data Operation = Add | Sub

-- simple assignment type for llvm
data Assignment = Bind (Name, Type, String) | Binary (Name, Type, Operation, String, String)

instance Show Operation where
  show Add      = "add "
  show Sub      = "sub "

instance Show Assignment where
  show (Bind (n, t, v)) = ((show n) ++ " = " ++ (show t) ++ v ++ "\n")
  show (Binary (n, t, o, s1, s2)) = ((show n) ++ " = " ++ show o ++ (show t) ++ s1 ++ ", " ++ s2 ++ "\n")
