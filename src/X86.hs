module X86 where

import Data.Char


data Val
    = VInt Integer
    | VDoub Double
    | VVal Var    
    deriving (Eq)

instance Show Val where
    show = showVal

data Reg = EAX | EBX | ECX | EDX

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
    | DPush Val 
   -- | Load Size Val
   -- | Store Size Val Size Val
    | Ass Val Instruction
    | Mul Val Val
    | FMul Val
    | Div Val
    | Div2 Size Val
    | FDiv Val
    | Add Val Val
    | FAdd Val
    | Sub Val Val
    | FSub Val
    | Inc Size Val
    | Dec Size Val
    | Neg Val
    | FNeg
    | Not Val
    | Mod Size Val Val
    | Compare Val Val
    | Compare2 Size Val Val
    | FCompare Val
    | And Val Val
    | Or Val Val
    | Return
    | Goto Label
    | CondB Val Label
    | Comment String
    | Raw String
    | Invoke String 
    | FFree Int
    | Fld Val
    | Fxch Val
    | Fst Val
    | Fldz
    | Fld1
    | Cld
    deriving (Eq)

showInstruction :: Instruction -> String 
showInstruction (Move v1 v2)        = "mov " ++ show v1 ++ ", " ++ show v2
showInstruction (Move2 s v1 v2)     = "mov " ++ showSize s ++ " " ++ show v1 ++ ", " ++ show v2
showInstruction (Push v)            = "push " ++ show v
showInstruction (Push2 s v)         = "push " ++ showSize s ++ " " ++ show v
showInstruction (DPush v)         = "fstp " ++ show v
--showInstruction (Load s v)          = "load " ++ showSize s ++ "* " ++ show v
--showInstruction (Store s1 v1 s2 v2) = "store " ++ showSize s1 ++ " " ++ show v1 ++ " , " ++ showSize s2 ++ "* " ++ show v2 
showInstruction (Ass v1@(VVal v1') i)  | head v1'=='%' =  show v1 ++ " = " ++ showInstruction i
                                        | otherwise = "%" ++ show v1 ++ " = " ++ showInstruction i
showInstruction (Mul v1 v2)         = "imul " ++ show v1 ++ ", " ++ show v2
showInstruction (FMul v)           = "fmul " ++ show v
--showInstruction (Div DWord v1 v2)   = "fdiv " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Div v1)            = "idiv " ++ show v1
showInstruction (Div2 s v1)         = "idiv " ++ showSize s ++ " " ++ show v1
showInstruction (FDiv v)            = "fdiv " ++ show v
showInstruction (Add v1 v2)         = "add " ++ show v1 ++ ", " ++ show v2
showInstruction (FAdd v1)           = "fadd " ++ show v1
showInstruction (Sub v1 v2)         = "sub " ++ show v1 ++ ", " ++ show v2
showInstruction (FSub v1)           = "fsub " ++ show v1
showInstruction (Inc s v)            = "inc " ++ showSize s++ " " ++ show v
showInstruction (Dec s v)            = "dec " ++ showSize s++ " " ++ show v
--showInstruction (Neg DWord v)       = "fsub " ++ showSize DWord ++ " 0.0, " ++ show v
showInstruction (Neg v)             = "neg " ++ show v
showInstruction (FNeg)              = "fchs"
showInstruction (Not v)             = "not " ++ show v
--showInstruction (Mod s v1 v2)       = "srem " ++ showSize s ++ " " ++ show v1 ++ " , " ++ show v2
--showInstruction (Compare c DWord v1 v2) = "fcmp " ++ (showCond DWord c) ++ " " ++ showSize DWord ++ " " ++ show v1 ++ " , " ++ show v2
showInstruction (Compare v1 v2)     = "cmp " ++ show v1 ++ ", " ++ show v2
showInstruction (Compare2 s v1 v2)  = "cmp " ++ showSize s ++ " " ++ show v1 ++ ", " ++ show v2
showInstruction (FCompare v1)       = "fcom "++ show v1 
showInstruction (And v1 v2)         = "test " ++ show v1 ++ ", " ++ show v2
showInstruction (Or v1 v2)          = "or " ++ show v1 ++ " , " ++ show v2
showInstruction (Return)            = "leave \nret" 
showInstruction (Goto l)            = "jmp L" ++ show l
showInstruction (CondB c l)         = (showCond c) ++ " L" ++ show l
showInstruction (Comment s)         = ";" ++ s
showInstruction (Raw s)             = s
showInstruction (Invoke f)          = "call " ++ f
showInstruction (FFree i)           = "ffree st" ++ show i
showInstruction (Fld v)             = "fld qword " ++ show v
showInstruction (Fxch v)            = "fxch " ++ show v 
showInstruction (Fst v)             = "fst qword " ++ show v 
showInstruction (Fldz)              = "fldz" 
showInstruction (Fld1)              = "fld1" 
showInstruction (Cld)               = "cld" 



showSize :: Size -> String
showSize Bit        = "dword"
showSize Byte       = "dword"
showSize Word       = "dword"
showSize DWord      = "qword"
showSize Void       = "void"
showSize (SSize s)  = s
showSize (P s)      = showSize s ++ "*"
showSize (A s i)    =  "[ " ++ show i ++ " x " ++ showSize s ++ " ]"

showVal :: Val -> String
showVal (VVal s) = s
showVal (VDoub d)= show d
showVal (VInt i) = show i


showCond :: Val -> String
{-showCond DWord Eq = "ueq"
showCond DWord Ne = "une"
showCond DWord Sgt = "ugt"
showCond DWord Sge = "uge"
showCond DWord Slt = "ult"
showCond DWord Sle = "ule"-}
showCond c =[toLower (head (show c))] ++ tail (show c)






