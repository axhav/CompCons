module LLVM where

data Val
    = VInt Integer
    | VVal Var    
    deriving (Show, Eq)

type Label = String
type Var = String
type Pointer = Size Val
type Cond = Equ | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle

data Size = Bit | Byte | Word | DWord | Void
  deriving (Eq)

data Instruction
    = Load Size Val
    | Store Size Val Size Val
    | Alloca Size
    | Mul Size Val Val
    | Div Size Val Val
    | Add Size Val Val
    | Sub Size Val Val
    | Compare Cond Size Val Val
    | And Size Val Val
    | Or Size Val Val
    | Return Size Val
    | Goto Label
    | CondB Val Label Label
    | Comment String
    | Raw String
    | Invoke Size String 
    deriving (Eq)

showInstruction :: Instruction -> String 
showInstruction (Load s v)          = "load " ++ show s ++ "* " ++ show v
showInstruction (Store s1 v1 s2 s2) = "store " ++ show s1 ++ " " ++ show v1 ++ " , " ++ show s2 ++ "* " ++ show v2 
showInstruction (Alloca s)          = "alloca " ++ show s
showInstruction (Mul s v1 v2)       = "mul " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div s v1 v2)       = "div " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Add s v1 v2)       = "add " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Sub s v1 v2)       = "sub " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare c s v1 v2) = "icmp " ++ show c ++ " " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (And s v1 v2)       = "and " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Or s v1 v2)        = "or " ++ show s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Return s v)        = "ret " ++ show s ++ " " ++ show v
showInstruction (Goto l)            = "br " ++ show l
showInstruction (CondB v l1 l2)     = "br i1 " ++ show v ++ " , " ++ show l1 ++ " , " ++ show l2
showInstruction (Comment s)         = ";" ++ s
showInstruction (Raw s)             = s
showInstruction (Invoke s f)        = "call " ++ show s ++ " " ++ f


