module Bindings ( module Bindings ) where
import Types (LLVMTypes(..), Name(..), Token, Scope(..))

-- | A binding here is a defined as any referencable name tied to some defined data.
-- here a method counts as data as does a contstant and anything in between. If the data
-- has a name by which to reference it, it is then bound to that name
data Binding = Variable (Scope, Name, LLVMTypes, Value) deriving Eq

instance Show Binding where
 show (Variable (l, n, _, I v)) = show l ++ show n ++ " = " ++ show v
 show (Variable (l, n, t, v))   = show l ++ show n ++ " = " ++ show t ++ " " ++ show v

-- Values constitue any data that is given as the argument to a statement of the form '<scope> <name> = <data> <type> <data>'
-- These values are sorted and stored as different data structures based on the datatype and the context

-- | F for function definitions
-- C for Constants 'i32 0'
-- V for variable names
-- E for Value expression
data Value = C String | V Name | I Instruction deriving Eq

-- | an instruction list that utilizes the
-- - basic operations
-- - control flow delegation
-- old version
-- type Instruction = String
data Instruction  = BOps BinOps | UnOps UnaryOps | BitOps BitOps | CallFxn CallOp deriving Eq

data BinOps       = Add Token Token | Sub Token Token | Mul Token Token | Div Token Token | Rem Token Token deriving Eq
data UnaryOps     = Neg Token deriving Eq
data BitOps       = LShift Token Token | RShift Token Token | And Token Token | Or Token Token | XOr Token Token deriving Eq
data CallOp       = Call LLVMTypes String [Token] deriving Eq

instance Show Instruction where
 show (BOps b) = show b
 show (UnOps u) = show u
 show (BitOps b) = show b
 show (CallFxn c) = show c

instance Show Value where
 show (C s) = s
 show (V n) = show n
 show (I i) = show i
 
instance Show BitOps where
 show (LShift (t1, l1, n1) (_, l2, n2)) = "shl "   ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2
 show (RShift (t1, l1, n1) (_, l2, n2)) = "lshr "  ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2
 show (And    (t1, l1, n1) (_, l2, n2)) = "and "   ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2
 show (Or     (t1, l1, n1) (_, l2, n2)) = "or "    ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2
 show (XOr    (t1, l1, n1) (_, l2, n2)) = "xor "   ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2

instance Show UnaryOps where
  show (Neg (t1, L, Name (0, l1))) = "fneg " ++ show t1 ++ " " ++ l1

binOpsPrint :: LLVMTypes -> Scope -> Name -> Scope -> Name -> String -> String
binOpsPrint t1 l1 n1 l2 n2 op = op ++ " " ++ show t1 ++ " " ++ show l1 ++ show n1 ++ ", " ++ show l2 ++ show n2


instance Show BinOps where
  show (Add (t1, l1, n1) (_, l2, n2)) =  if t1 == LLVMFloat || t1 == LLVMDouble then binOpsPrint t1 l1 n1 l2 n2 "fadd" else binOpsPrint t1 l1 n1 l2 n2 "add"
  show (Sub (t1, l1, n1) (_, l2, n2)) =  if t1 == LLVMFloat || t1 == LLVMDouble then binOpsPrint t1 l1 n1 l2 n2 "fsub" else binOpsPrint t1 l1 n1 l2 n2 "sub"
  show (Mul (t1, l1, n1) (_, l2, n2)) = if t1 == LLVMFloat || t1 == LLVMDouble then binOpsPrint t1 l1 n1 l2 n2 "fmul" else binOpsPrint t1 l1 n1 l2 n2 "mul"
  show (Div (t1, l1, n1) (_, l2, n2)) = if t1 == LLVMFloat || t1 == LLVMDouble then binOpsPrint t1 l1 n1 l2 n2 "fdiv" else binOpsPrint t1 l1 n1 l2 n2 "div"
  show (Rem (t1, l1, n1) (_, l2, n2)) = if t1 == LLVMFloat || t1 == LLVMDouble then binOpsPrint t1 l1 n1 l2 n2 "frem" else binOpsPrint t1 l1 n1 l2 n2 "srem"

instance Show CallOp where
  show (Call typ name pl) = "call " ++ show typ ++ " @" ++ name ++ "(" ++ unwrap pl ++")"
    where
    unwrap :: [Token] -> String
    unwrap = foldr (\(t, s, n) acc -> if length acc < 1 then show t ++ " " ++ show s ++ show n ++ acc else show t ++ " " ++ show s ++ show n ++ ", " ++ acc ) ""
