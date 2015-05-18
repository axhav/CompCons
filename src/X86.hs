module X86 where

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
--- Jz = eq | Jnz = !eq | e = eq
-- a and b unsiged, a greater, b lesser 
-- g and l signed, g greater, l lesser
data Cond = Jz | Jnz | Ja | Jae | Jb | Jbe | Jg | Jge | Jl | Jle
    deriving (Show,Eq)

data Size = Bit | Byte | Word | DWord | Void | SSize String | P Size | A Size Int
    deriving (Eq)

data Instruction
    = Move Val Val
    | Move2 Size Val Val
    | Push Val 
    | Push2 Size Val 
   -- | Load Size Val
   -- | Store Size Val Size Val
    | Ass Val Instruction
    | Mul Val Val
    | Div Val Val
    | Add Val Val
    | Sub Val Val
    | Neg Val
    | Not Val
    | Mod Size Val Val
    | Compare Val Val
    | And Val Val
    | Or Val Val
    | Return
    | Goto Label
    | CondB Size Cond Label
    | Comment String
    | Raw String
    | Invoke String 
    deriving (Eq)

showInstruction :: Instruction -> String 
showInstruction (Move v1 v2)        = "mov " ++ show v1 ++ ", " ++ show v2
showInstruction (Move2 s v1 v2)     = "mov " ++ showSize s ++ " " ++ show v1 ++ ", " ++ show v2
showInstruction (Push v)            = "push " ++ show v
showInstruction (Push2 s v)         = "push " ++ showSize s ++ " " ++ show v
--showInstruction (Load s v)          = "load " ++ showSize s ++ "* " ++ show v
--showInstruction (Store s1 v1 s2 v2) = "store " ++ showSize s1 ++ " " ++ show v1 ++ " , " ++ showSize s2 ++ "* " ++ show v2 
showInstruction (Ass v1@(VVal v1') i)  | head v1'=='%' =  show v1 ++ " = " ++ showInstruction i
                                        | otherwise = "%" ++ show v1 ++ " = " ++ showInstruction i
--showInstruction (Mul DWord v1 v2)   = "fmul " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Mul v1 v2)         = "imul " ++ show v1 ++ ", " ++ show v2
--showInstruction (Div DWord v1 v2)   = "fdiv " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div v1 v2)         = "sdiv " ++ show v1 ++ ", " ++ show v2
showInstruction (Add v1 v2)         = "add " ++ show v1 ++ ", " ++ show v2
showInstruction (Sub v1 v2)         = "sub " ++ show v1 ++ ", " ++ show v2
--showInstruction (Neg DWord v)       = "fsub " ++ showSize DWord ++ " 0.0, " ++ show v
showInstruction (Neg v)             = "neg " ++ show v
showInstruction (Not v)             = "not " ++ show v
--showInstruction (Mod s v1 v2)       = "srem " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
--showInstruction (Compare c DWord v1 v2) = "fcmp " ++ (showCond DWord c) ++ " " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare v1 v2)     = "cmp " ++ show v1 ++ ", " ++ show v2
showInstruction (And v1 v2)         = "test " ++ show v1 ++ ", " ++ show v2
showInstruction (Or v1 v2)          = "or " ++ show v1 ++ " , " ++ show v2
showInstruction (Return)            = "leave \nret" 
showInstruction (Goto l)            = "jmp" ++ show l
showInstruction (CondB s c l)       = (showCond s c) ++ " " ++ show l
showInstruction (Comment s)         = ";" ++ s
showInstruction (Raw s)             = s
showInstruction (Invoke f)          = "call " ++ f


showSize :: Size -> String
showSize Bit        = "i1"
showSize Byte       = "i8"
showSize Word       = "dword"
showSize DWord      = "TODO"
showSize Void       = "void"
showSize (SSize s)  = s
showSize (P s)      = showSize s ++ "*"
showSize (A s i)    =  "[ " ++ show i ++ " x " ++ showSize s ++ " ]"

showVal :: Val -> String
showVal (VVal s) = s
showVal (VDoub d)= show d
showVal (VInt i) = show i


showCond :: Size -> Cond -> String
{-showCond DWord Eq = "ueq"
showCond DWord Ne = "une"
showCond DWord Sgt = "ugt"
showCond DWord Sge = "uge"
showCond DWord Slt = "ult"
showCond DWord Sle = "ule"-}
showCond _ c =[toLower (head (show c))] ++ tail (show c)






