module Lib ( module Lib ) where
import Types (LLVMTypes, Name)

type ValueLexeme = String

data Scope = L | G deriving Eq

instance Show Scope where
  show L = "%"
  show G = "@"

-- | A binding here is a defined as any referencable name tied to some defined data.
-- here a method counts as data as does a contstant and anything in between. If the data
-- has a name by which to reference it, it is then bound to that name
data Binding = ConstVar (Scope, Name, LLVMTypes, ValueLexeme) deriving Eq

instance Show Binding where
 show (ConstVar (l, n, t, v)) = show l ++ show n ++ " = " ++ show t ++ " " ++ v
