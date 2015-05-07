module LLVM where

import Data.Char


data Val
    = VInt Integer
    | VDoub Double
    | VVal Var    
    deriving (Eq)

instance Show Val where
    show = showVal


type Label = Int
type Var = String
data Cond = Eq | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle
    deriving (Show,Eq)

data Size = Bit | Byte | Word | DWord | Void | SSize String | P Size | A Size Int
    deriving (Eq)

data Instruction
    = Load Size Val
    | Store Size Val Size Val
    | Alloca Size
    | Calloc Int Size
    | Ass Val Instruction
    | Mul Size Val Val
    | Div Size Val Val
    | Add Size Val Val
    | Sub Size Val Val
    | Neg Size Val
    | Not Val
    | Mod Size Val Val
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
    | GString Val Int Val
    | GStruct Size String
    | TwoArray Int Val Int Int
    | PtrToInt Size Val Size
    | BitCast Size Val Size
    | GetElmPtr Size Val Integer Val
    deriving (Eq)

showInstruction :: Instruction -> String 
showInstruction (Load s v)          = "load " ++ showSize s ++ "* " ++ show v
showInstruction (Store s1 v1 s2 v2) = "store " ++ showSize s1 ++ " " ++ show v1 ++ " , " ++ showSize s2 ++ "* " ++ show v2 
showInstruction (Alloca s)          = "alloca " ++ showSize s
showInstruction (Calloc n s)        = "call " ++ showSize s ++ "* @calloc(i32" ++ show n ++ ", i32 " ++ showSize s ++ ")"  
showInstruction (Ass v1@(VVal v1') i)  | head v1'=='%' =  show v1 ++ " = " ++ showInstruction i
                                        | otherwise = "%" ++ show v1 ++ " = " ++ showInstruction i
showInstruction (Mul DWord v1 v2)   = "fmul " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Mul s v1 v2)       = "mul " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div DWord v1 v2)   = "fdiv " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div s v1 v2)       = "sdiv " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Add DWord v1 v2)   = "fadd " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Add s v1 v2)       = "add " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Sub DWord v1 v2)   = "fsub " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Sub s v1 v2)       = "sub " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Neg DWord v)       = "fsub " ++ showSize DWord ++ " 0.0, " ++ show v
showInstruction (Neg s v)           = "sub " ++ showSize s ++ " 0, " ++ show v
showInstruction (Not v)             = "xor i1 1 , " ++ show v
showInstruction (Mod s v1 v2)       = "srem " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare c DWord v1 v2) = "fcmp " ++ (showCond DWord c) ++ " " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare c s v1 v2) = "icmp " ++ (showCond s c) ++ " " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (And s v1 v2)       = "and " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Or s v1 v2)        = "or " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Return s v)        = "ret " ++ showSize s ++ " " ++ show v
showInstruction (VReturn)           = "ret void"
showInstruction (Goto l)            = "br label %L" ++ show l
showInstruction (CondB v l1 l2)     = "br i1 " ++ show v ++ " , label %L" ++ show l1 ++ " , label %L" ++ show l2
showInstruction (Comment s)         = ";" ++ s
showInstruction (Raw s)             = s
showInstruction (Invoke s f)        = "call " ++ showSize s ++ " " ++ f
showInstruction (GString v1 i v2)   = show v1 ++ " = internal constant [" ++ show i ++ " x i8] c\"" ++ show v2 ++ "\\00\"" 
showInstruction (GStruct s n)       = n ++ " = type " ++ n ++ "Struct*\n" ++ n ++ "Struct = type { i32, [0 x " ++ showSize s ++ "]* }"
showInstruction (TwoArray i1 v i2 i3) = "getelementptr [" ++ show i1 ++ " x i8]* " ++ show v ++ " , i32 " ++ show i2 ++ " , i32 " ++ show i3
showInstruction (PtrToInt s1 v2 s2) = "ptrtoint " ++ showSize s1 ++ "* " ++ show v2 ++ " to " ++ showSize s2
showInstruction (BitCast s1 v2 s2)  = "bitcast " ++ showSize s1 ++ " " ++ show v2 ++ " to " ++ showSize s2
showInstruction (GetElmPtr s1 v i1 v2) = "getelementptr " ++ showSize s1 ++ " " ++ show v ++ ", i32 " ++ show i1 ++ " , i32 " ++ show v2


showSize :: Size -> String
showSize Bit        = "i1"
showSize Byte       = "i8"
showSize Word       = "i32"
showSize DWord      = "double"
showSize Void       = "void"
showSize (SSize s)  = s
showSize (P s)      = showSize s ++ "*"
showSize (A s i)    =  "[ " ++ show i ++ " x " ++ showSize s ++ " ]"

showVal :: Val -> String
showVal (VVal s) = s
showVal (VDoub d)= show d
showVal (VInt i) = show i


showCond :: Size -> Cond -> String
showCond DWord Eq = "ueq"
showCond DWord Ne = "une"
showCond DWord Sgt = "ugt"
showCond DWord Sge = "uge"
showCond DWord Slt = "ult"
showCond DWord Sle = "ule"
showCond _ c =[toLower (head (show c))] ++ tail (show c)






