module CodeGenX86 where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Char

import AbsJavalette
import PrintJavalette
import LexJavalette
import ParJavalette

import qualified X86

-- | Environment (handled by state monad)
data Env = Env
  { className  :: String
  , envSig     :: Sig
  , contexts   :: Contexts
  , code       :: [X86.Instruction]
  , label      :: X86.Label
  , tempReg    :: Int
  , globalData :: [X86.Instruction]
  , globalText :: [X86.Instruction]
  }

type Contexts = [VarContext]

-- | Variables in scope (symbol table)
data VarContext = VarContext
  { vars :: Map Ident (X86.Val,Type)
  , next :: X86.Val
  }

type Sig = Map Ident TopDef
  
type CodeGen = State Env


-- * Service functions

-- Adds the input instruction to generated code 
emit :: X86.Instruction -> CodeGen ()
emit i = modify $ updateCode $ (i :)

-- Adds a comment to the generated code
comment :: String -> CodeGen ()
comment = emit . X86.Comment

-- Adds a newline to the generated code
blank :: CodeGen ()
blank = emit $ X86.Raw ""

-- Creates a new block.
newBlock :: CodeGen ()
newBlock = do
    cont <- gets contexts
    case cont of
        [] -> modify $ updateContexts $ (emptyContext :)
        _ ->do
            let c@VarContext{next=(X86.VInt n)} = head cont
            modify $ updateContexts $ (almostemptyContext (n) :)

-- Ends current block.
exitBlock :: CodeGen ()
exitBlock = do  
    cnt <- gets contexts 
    case cnt of 
        (c:c1:cont) -> do
            let n = next c
            let newTail = c1 {next = n} : cont
            modify $ updateContexts $ (\x -> newTail)
        (c:[]) ->    
            modify $ updateContexts $ tail
    

extendContext :: Ident -> Type -> CodeGen ()
extendContext x t = modify $ updateContexts $ \ (b : bs) ->
  b { vars = Map.insert x (next b,t) (vars b)
    , next = (valAdd (next b) 1)
    } : bs
    
extendContextvVal :: Ident -> Type -> X86.Val -> CodeGen ()
extendContextvVal id t v = modify $ updateContexts $ \ (b : bs) ->
  b { vars = Map.insert id (v,t) (vars b)
    } : bs

-- Increment reg/label counter.    
valAdd :: X86.Val -> Integer -> X86.Val
valAdd (X86.VInt i) i1 = X86.VInt (i+i1)

extendEnvSig :: TopDef -> CodeGen () 
extendEnvSig def@(FnDef _t i _args _ss) = modify $ updateEnvSig i def

-- Looks up the input variable and return the Val and type of variable.
lookupVar :: Ident -> CodeGen (X86.Val,Type)
lookupVar x = do
  bs <- gets contexts
  case catMaybes $ map (Map.lookup x . vars) bs of
    []      -> error $ "unbound var " ++ printTree x
    (a : _) -> return a

-- Returns the next label number.    
getNextLabel :: CodeGen X86.Label
getNextLabel = do
    l <- gets label
    modify $ updateLabel (+1) 
    return l 

-- Return the next temp register number.
getNextTempReg :: CodeGen X86.Val
getNextTempReg = undefined --do
    

-- Returns the register number for a variable.    
getVarReg :: Ident -> CodeGen X86.Val
getVarReg id = undefined --do

-- Insert new global variable into globalList and return the variable name.
setNextGlobalVar :: String -> CodeGen X86.Val
setNextGlobalVar s = undefined --do

-- Adds new global data into list and returns the name of created data
addGlobalData:: String -> CodeGen X86.Val
addGlobalData s = do
    gD <- gets globalData
    when (gD == []) $ modify $ updateGlobalData ((X86.Raw "segment .data"):)
    gD <- gets globalData
    let gName = "str" ++ show (length gD)
    modify $ updateGlobalData ((X86.Raw (gName ++ " db " ++ show s)):)
    return $ X86.VVal gName

-- Adds new global text into list
addGlobalText :: String -> CodeGen ()
addGlobalText s = do
    modify $ updateGlobalText ((X86.Raw ("global" ++ s)):)
       
 
-- * Environment

emptyEnv :: Env
emptyEnv = Env
  { className  = ""
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  , tempReg    = 0
  , globalData = []
  , globalText = [X86.Raw "segment .text\n"]
  }

updateEnvSig :: Ident -> TopDef -> Env -> Env 
updateEnvSig i def env = env {envSig = Map.insert i def (envSig env)}
  
updateContexts :: (Contexts -> Contexts) -> Env -> Env
updateContexts f env = env { contexts = f (contexts env) }

updateCode :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateCode f env = env { code = f (code env) }

updateLabel :: (X86.Label -> X86.Label) -> Env -> Env
updateLabel f env = env { label = f ( label env)}

updateTempReg :: (Int -> Int) -> Env -> Env
updateTempReg f env = env { tempReg = f ( tempReg env)}

updateGlobalData :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateGlobalData f env = env { globalData = f (globalData env) }

updateGlobalText :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateGlobalText f env = env { globalText = f (globalText env) }

-- * Contexts

emptyContext :: VarContext
emptyContext = VarContext
  { vars = Map.empty
  , next = X86.VInt 0
  }

almostemptyContext :: Integer -> VarContext
almostemptyContext i = VarContext
  { vars = Map.empty
  , next = X86.VInt i
  }


-- * Code Generator

-- Generats the code and returns it as a string.
codeGen :: Program -> String
codeGen prg = header ++ unlines (map X86.showInstruction lcode)
    where
        compileCode = compileProgram prg `execState` emptyEnv
        lcode = reverse $ code compileCode ++ globalText compileCode ++ globalData compileCode
        header = unlines (["extern printInt", "extern printDouble", "extern printString", 
                        "extern readInt", "extern readDouble",""]) 

-- Compiles code for each method.
compileProgram :: Program -> CodeGen ()
compileProgram (Program defs) = do
    mapM_ extendEnvSig defs
    mapM_ compileDef defs

-- Compiles the code for a method.
compileDef :: TopDef -> CodeGen ()
compileDef (FnDef t id'@(Ident id) args b@(Block ss)) = do
    newBlock     
    args' <- showA args
    emit $ X86.Raw $ "define " ++ X86.showSize (typeToItype t) ++ " @"++ id ++ "(" ++ args' ++ ") {"
    allocateArgs args
    case t of
         Void -> do -- For adding return stament in void functions.
            case drop ((length ss) - 1) ss of
                [VRet] -> compileBlock b
                _      -> do
                    let b = Block (ss ++ [VRet])
                    compileBlock b
         _    -> do -- For checking if last statment was a condition.
            compileBlock b
            let ssl = last ss
            case ssl of
                Ret _ -> blank
                _     -> emit $ X86.Raw $ "unreachable"
    exitBlock
    emit $ X86.Raw $ "}"

-- Compiles the code for all the statments within a block 
compileBlock :: Block -> CodeGen ()
compileBlock (Block ss) = do   
    mapM_ compileStm ss

-- Compiles a statment.
compileStm :: Stmt -> CodeGen ()
compileStm s = do
    case s of
        Empty -> do
            blank   
        (BStmt b) -> do
            newBlock 
            compileBlock b
            exitBlock
        (Decl t i) -> undefined --do
        (Ass e1 expr@(ETyped e t)) -> undefined --do
        (Incr id) -> undefined --do
        (Decr id) -> undefined --do
        (Ret expr@(ETyped e t)) -> undefined --do
        (VRet) -> undefined --do
        (Cond expr@(ETyped e' t) stm) -> undefined --do
        (CondElse expr@(ETyped e' t) stm1 stm2) -> undefined --do
        (While expr@(ETyped e' t) stm) -> undefined -- do
        (SExp expr) -> undefined --do

-- Compiles a expression
compileExp :: Expr -> CodeGen X86.Val
compileExp (ETyped (ELitTrue) t) = return $ X86.VInt 1
compileExp (ETyped (ELitFalse) t) = return $ X86.VInt 0
compileExp (ETyped (ELitInt i) t) = return $ X86.VInt i
compileExp (ETyped (ELitDoub d) t) = return $ X86.VDoub d
compileExp (ETyped (EVar id) t) = undefined --case t of
compileExp (ETyped (EApp id'@(Ident id) exps) t) = undefined --do
compileExp (ETyped (EString s) t) = do
    nameS <- addGlobalData s
    emit $ X86.Push nameS
    emit $ X86.Invoke "printString"
    return $ X86.VVal "No!" --TODO Should not return anything?
compileExp (ETyped (Neg e) t) = undefined --do
compileExp (ETyped (Not e) t) = undefined --do
compileExp (ETyped (EMul e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of
        Times -> undefined --do
        Div   -> undefined --do
        Mod   -> undefined --do 
compileExp (ETyped (EAdd e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of 
        Plus  -> undefined --do
        Minus -> undefined --do 
compileExp (ETyped (ERel e1@(ETyped e1' t) o e2) t') = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    r <- getNextTempReg
    case o of 
        LTH -> undefined --do
        LE  -> undefined --do
        GTH -> undefined --do
        GE  -> undefined --do
        EQU -> undefined --do
        NE  -> undefined --do
compileExp (ETyped (EAnd e1 e2) t) = undefined --do
compileExp (ETyped (EOr e1 e2) t) = undefined --do
compileExp a = fail $ printTree a


-- * Helps functions for the code generator.

-- Returns the size of type inside a register.
getHardwareSizeOfType :: Type -> CodeGen X86.Val
getHardwareSizeOfType t = undefined --do

-- Generats and returns the code for the arguments to a code block/method.
showA :: [Arg] -> CodeGen String
showA []           = return ""
showA ([Arg t id]) = undefined --do
showA ((Arg t id):ids) = undefined --do


-- Generats and returns the code for the arguments/input variables for method calls
showE :: [Type] -> [X86.Val] ->  String
showE _ []              = "" 
showE [Void] ([X86.VVal s]) = "i8* " ++ s  
showE [t] ([X86.VVal s]) = (X86.showSize (typeToItype t)) ++ " " ++ s
showE [t] ([X86.VInt i]) = (X86.showSize (typeToItype t)) ++ " " ++ show i
showE [t] ([X86.VDoub d]) = (X86.showSize (typeToItype t)) ++ " " ++ show d
showE (Void:ts) ((X86.VVal s):ss) = "i8* " ++ s ++ showE ts ss
showE (t:ts) ((X86.VVal s):ss) = (X86.showSize (typeToItype t)) ++ " " ++ s ++ " , " ++ showE ts ss
showE (t:ts) ((X86.VInt i):is) = (X86.showSize (typeToItype t)) ++ " " ++ show i ++ " , " ++ showE ts is
showE (t:ts) ((X86.VDoub d):ds) = (X86.showSize (typeToItype t)) ++ " " ++ show d ++ " , " ++ showE ts ds

-- Saves/allocates the arguemnts register onto the stack.
allocateArgs :: [Arg] -> CodeGen ()
allocateArgs [] = return ()
allocateArgs [Arg t id] = undefined --case t of
allocateArgs ((Arg t id):args) = undefined --case t of

-- Contverts from Type to X86 types.
typeToItype :: Type -> X86.Size
typeToItype Int  = X86.Word
typeToItype Doub = X86.DWord
typeToItype Bool = X86.Bit
typeToItype Void = X86.Void
typeToItype (ArrayT t _) = typeToItype t

        
-- Helps declar in the function "compileStm" to decide if variable is Initials or not 
declHelper :: Item -> Type -> CodeGen ()
declHelper (NoInit id) t = undefined --do
declHelper (Init id expr) t = undefined --do


    
    
    

   
