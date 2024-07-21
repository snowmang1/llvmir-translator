module Bindings ( module Bindings ) where
import Types (LLVMTypes(..), Name, Token)

type ValueLexeme = String

data Scope = L | G deriving Eq

instance Show Scope where
  show L = "%"
  show G = "@"

-- | A binding here is a defined as any referencable name tied to some defined data.
-- here a method counts as data as does a contstant and anything in between. If the data
-- has a name by which to reference it, it is then bound to that name
data Binding = Variable (Scope, Name, LLVMTypes, ValueLexeme) deriving Eq

instance Show Binding where
 show (Variable (l, n, t, v)) = show l ++ show n ++ " = " ++ show t ++ " " ++ v

-- Values constitue any data that is given as the argument to a statement of the form '<scope> <name> = <data> <type> <data>'
-- These values are sorted and stored as different data structures based on the datatype and the context

-- | F for function definitions
-- C for Constants 'i32 0'
-- V for variable names
-- E for Value expression
data Value = C String | V Name | E Instruction

-- | an instruction list that utilizes the
-- - basic operations
-- - control flow delegation
-- old version
-- type Instruction = String
data Instruction  = BOps BinOps | UnOps UnaryOps | BitOps BitOps | CallFxn CallOp

data BinOps       = Add Token Token | Sub Token Token | Mul Token Token | Div Token Token | Rem Token Token
data UnaryOps     = Neg Token
data BitOps       = LShift Token | RShift Token | And Token Token | Or Token Token | XOr Token Token
data CallOp       = Call LLVMTypes String [Token]

instance Show BinOps where
  show (Add (t1, l1) (_, l2)) = if t1 == LLVMFloat || t1 == LLVMDouble then "fadd " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2 else "add " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2
  show (Sub (t1, l1) (_, l2)) = if t1 == LLVMFloat || t1 == LLVMDouble then "fsub " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2 else "sub " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2
  show (Mul (t1, l1) (_, l2)) = if t1 == LLVMFloat || t1 == LLVMDouble then "fmul " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2 else "mul " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2
  show (Div (t1, l1) (_, l2)) = if t1 == LLVMFloat || t1 == LLVMDouble then "fdiv " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2 else "div " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2
  show (Rem (t1, l1) (_, l2)) = if t1 == LLVMFloat || t1 == LLVMDouble then "frem " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2 else "srem " ++ show t1 ++ " " ++ l1 ++ ", " ++ l2

instance Show CallOp where
  show (Call typ name pl) = "call " ++ show typ ++ " @" ++ name ++ "(" ++ unwrap pl ++")"
    where
    unwrap :: [(LLVMTypes, String)] -> String
    unwrap = foldr (\(t,n) acc -> if length acc < 1 then show t ++ " " ++ n ++ acc else show t ++ " " ++ n ++ ", " ++ acc ) ""
