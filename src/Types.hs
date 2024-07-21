module Types ( module Types ) where

-- | comprehensive list of supported types with Haddock compatible documentation WIP
-- Numeric types includes long, int, double, and float
-- Character type ==> i8
-- Boolean type   ==> i1
-- PrimType is a Handle for the primative types defined by the library
-- Aggregations ==> Arrays & Vectors
--
-- TODO:
-- * possible type for block assembly
-- type for block LLVM IR

-- | this Token is the combination of a lexeme and a type in proper Token form
type Token        = (LLVMTypes, String)

-- | the name type is for use inside the library only and will not be available outside of it as the
-- name type is used to keep track of a variables reference-able lexeme as well as its SSA compliant mangled binding.
-- Keeps track of what number reference this lexeme is.
-- This type will have an accompanying accompanying function mangle to assist with SSA compliance (only used on show)
data Name = Lexeme Integer String

-- | naive SSA compliance
mangle :: Name -> String
mangle (Lexeme i s) = s ++ show i

-- | defines the types that can be directly translated into LLVM IR type code.
-- we include the following types: Numeric (int, float, long, double), Boolean (i1), Character (i8), array & vector.
data LLVMTypes = Integer | LLVMFloat | Long | LLVMDouble | Boolean | Character | Array LLVMTypes Integer | Vector LLVMTypes Integer deriving Eq

instance Show LLVMTypes where
  show Integer = "i32"
  show LLVMFloat = "float"
  show Long = "i64"
  show LLVMDouble = "double"
  show Character = "i8"
  show Boolean = "i1"
  show (Array t x) = "[" ++ show x ++ " x " ++ show t ++ "]"

instance Show Name where
  show l = mangle l

instance Eq Name where
  (==) (Lexeme _ s1) (Lexeme _ s2) = (s1 == s2)
