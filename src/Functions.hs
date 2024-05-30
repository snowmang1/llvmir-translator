module Functions ( module Functions ) where

data Name = Global String | Local String

instance Show Name where
  show (Global s) = ("@" ++ s)
  show (Local s)  =("%" ++ s)

data Type = Number | Char

instance Show Type where
  show Number = "i32 "
  show Char   = undefined

data ParamList = Full [(Type, Name)] | Empty

instance Show ParamList where
  show (Full p) =
    expandList p 0 "" where
      expandList p 0 a = if (length p)==1 then (show_1 (head p)) else
        expandList (tail p) 1 (show_1 (head p)) where
          show_1 (ty,na) = show ty ++ show na
      expandList (h:t) i a = if (length t)<1 then (a ++ ", " ++ show_1 h) else
        expandList t (i+1) (a ++ ", " ++ show_1 h) where
          show_1 (ty,na) = show ty ++ show na

data LLVMFunction = Begin (Type, Name, ParamList) | EndFunction

instance Show LLVMFunction where
  show (Begin (t, n, p)) = ("define " ++ show t ++ show n ++ "(" ++ show p ++ ")" ++ " {\n")
  show (EndFunction) = "}\n"
