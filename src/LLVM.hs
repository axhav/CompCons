module LLVM where

data Val
    = VInt Integer
    | VDoub Double
    | VVal Var    
    deriving (Eq)

instance Show Val where
    show = showVal


type Label = Int
type Var = String
data Cond = Equ | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle
    deriving (Show,Eq)

data Size = Bit | Byte | Word | DWord | Void
    deriving (Eq)

data Instruction
    = Load Size Val
    | Store Size Val Size Val
    | Alloca Size
    | Ass Size Val Val
    | Mul Size Val Val
    | Div Size Val Val
    | Add Size Val Val
    | Sub Size Val Val
    | Compare Cond Size Val Val
    | And Size Val Val
    | Or Size Val Val
    | Return Size Val
    | VReturn
    | Goto Label
    | CondB Val Label Label
    | Comment String
    | Raw String
    | Invoke Size String 
    deriving (Eq)

showInstruction :: Instruction -> String 
showInstruction (Load s v)          = "load " ++ showSize s ++ "* " ++ show v
showInstruction (Store s1 v1 s2 v2) = "store " ++ showSize s1 ++ " " ++ show v1 ++ " , " ++ showSize s2 ++ "* " ++ show v2 
showInstruction (Alloca s)          = "alloca " ++ showSize s
showInstruction (Ass s v1@(VVal v1') v2)  | head v1'=='%' =  show v1 ++ " = " ++ showSize s ++" " ++ show v2
                                          | otherwise = "%" ++ show v1 ++ " = " ++ show v2
showInstruction (Mul s v1 v2)       = "mul " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div s v1 v2)       = "div " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Add s v1 v2)       = "add " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Sub s v1 v2)       = "sub " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare c s v1 v2) = "icmp " ++ show c ++ " " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (And s v1 v2)       = "and " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Or s v1 v2)        = "or " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Return s v)        = "ret " ++ showSize s ++ " " ++ show v
showInstruction (VReturn)           = "ret void"
showInstruction (Goto l)            = "br label %L" ++ show l
showInstruction (CondB v l1 l2)     = "br i1 " ++ show v ++ " , label %L" ++ show l1 ++ " , label %L" ++ show l2
showInstruction (Comment s)         = ";" ++ s
showInstruction (Raw s)             = s
showInstruction (Invoke s f)        = "call " ++ showSize s ++ " " ++ f


showSize :: Size -> String
showSize Bit = "i1"
showSize Byte = "i8"
showSize Word = "i32"
showSize DWord = "double"
showSize Void = "void"


showVal :: Val -> String
showVal (VVal s) = s
showVal (VDoub d)= show d
showVal (VInt i) = show i
