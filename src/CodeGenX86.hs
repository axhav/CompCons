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
  , tempReg    :: [X86.Val]
  , dTempReg   :: Int
  , stackP     :: Int
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
getNextTempReg :: Type ->  CodeGen X86.Val
getNextTempReg t = case t of
    Doub -> do
        r <- gets dTempReg
        modify $ updateDTempReg (+1)
        return $ X86.VVal ("st" ++ show r)
    _    -> do
        r <- gets tempReg
        modify $ updateTempReg tail
        return $ head r
    
resetTempReg  :: CodeGen ()
resetTempReg = do
    dt <- gets dTempReg
    case dt > 0 of 
        True -> loop dt
        False -> loop (-1)
    modify $ updateDTempReg (\x -> 0)
    modify $ updateTempReg (\x -> [X86.VVal "eax",X86.VVal "ebx",X86.VVal "ecx",X86.VVal "edx"])
    where 
        loop (-1) = return ()
        loop 0 = emit $ X86.FFree 0
        loop i = emit (X86.FFree i) >> loop (i-1)
        


-- Returns the register number for a variable.    
getVarReg :: Ident -> CodeGen X86.Val
getVarReg id = undefined --do

-- Insert new global variable into globalList and return the variable name.
setNextGlobalVar :: String -> CodeGen X86.Val
setNextGlobalVar s = undefined --do

-- Adds new global data into list and returns the name of created data
addGlobalData:: Type-> String -> CodeGen X86.Val
addGlobalData t s = do
    gD <- gets globalData
    when (gD == []) $ modify $ updateGlobalData ((X86.Raw "segment .data"):)
    gD <- gets globalData
    let gName = "str" ++ show (length gD)
    case t of
        Void ->  modify $ updateGlobalData ((X86.Raw (gName ++ " db " ++ show s ++ " , 0" )):)
        _    ->  modify $ updateGlobalData ((X86.Raw (gName ++ " dq " ++ s)):)
    return $ X86.VVal gName

-- Adds new global text into list
addGlobalText :: String -> CodeGen ()
addGlobalText s = modify $ updateGlobalText ((X86.Raw ("global " ++ s)):)
    
incStackPointer :: Int -> CodeGen ()
incStackPointer i = modify $ updateStackP (+i)
 
-- * Environment

emptyEnv :: Env
emptyEnv = Env
  { className  = ""
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  , tempReg    = [X86.VVal "eax",X86.VVal "ebx",X86.VVal "ecx",X86.VVal "edx"]
  , dTempReg   = 0
  , stackP     = 0
  , globalData = []
  , globalText = [X86.Raw "segment .text"]
  }

updateEnvSig :: Ident -> TopDef -> Env -> Env 
updateEnvSig i def env = env {envSig = Map.insert i def (envSig env)}
  
updateContexts :: (Contexts -> Contexts) -> Env -> Env
updateContexts f env = env { contexts = f (contexts env) }

updateCode :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateCode f env = env { code = f (code env) }

updateLabel :: (X86.Label -> X86.Label) -> Env -> Env
updateLabel f env = env { label = f ( label env)}

updateTempReg :: ([X86.Val] -> [X86.Val]) -> Env -> Env
updateTempReg f env = env { tempReg = f ( tempReg env)}

updateDTempReg :: (Int -> Int) -> Env -> Env
updateDTempReg f env = env { dTempReg = f ( dTempReg env)}

updateGlobalData :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateGlobalData f env = env { globalData = f (globalData env) }

updateGlobalText :: ([X86.Instruction] -> [X86.Instruction]) -> Env -> Env
updateGlobalText f env = env { globalText = f (globalText env) }

updateStackP :: (Int -> Int) -> Env -> Env
updateStackP f env = env { stackP = f ( stackP env)}

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
    addGlobalText id    
    emit $ X86.Raw $ id ++ ":"
    emit $ X86.Push (X86.VVal "dword ebp")
    emit $ X86.Move (X86.VVal "ebp") (X86.VVal "esp")
    allocateArgs args
    compileBlock b
    exitBlock

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
        (Decl t i) -> do
            for i
            resetTempReg
                where for (item:[]) = declHelper item t
                      for (item:items)= declHelper item t >> for items
        (Ass e1@(ETyped (EVar id) _) expr@(ETyped e t)) -> do
            e1' <- compileExp e1
            e2' <- compileExp expr
            let b2 = isMemoryVar e2'
            case t of
                Doub -> do
                    emit $ X86.Fxch e2'
                    emit $ X86.Fst e1'
                    emit $ X86.Fxch e2'
                _    -> do
                    case b2 of
                        True -> do
                            emit $ X86.Move (X86.VVal "eax") e2'
                            emit $ X86.Move2 (typeToItype t) e1' (X86.VVal "eax")            
                        False -> do
                            emit $ X86.Move2 (typeToItype t) e1' e2'
            resetTempReg           
        (Incr id) -> undefined --do
        (Decr id) -> undefined --do
        (Ret expr@(ETyped e t)) -> do
            expr' <- compileExp expr
            emit $ X86.Move (X86.VVal "eax") expr'
            emit $ X86.Return 
            resetTempReg          
        (VRet) -> emit $ X86.Return 
        (Cond expr@(ETyped e' t) stm) -> do
            l1 <- getNextLabel
            expr' <- compileExp expr 
            let b1 = isMemoryVar expr'
            case b1 of
                True -> emit $ X86.Compare2 (typeToItype t) expr' (X86.VInt 0)
                False -> emit $ X86.Compare expr' (X86.VInt 0)
            emit $ X86.CondB (X86.VVal "je") l1
            compileStm stm
            emit $ X86.Raw $ "L" ++ show l1 ++ ":"
            resetTempReg
        (CondElse expr@(ETyped e' t) stm1 stm2) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            expr' <- compileExp expr 
            let b1 = isMemoryVar expr'
            case b1 of
                True -> emit $ X86.Compare2 (typeToItype t) expr' (X86.VInt 0)
                False -> emit $ X86.Compare expr' (X86.VInt 0)
            emit $ X86.CondB (X86.VVal "je") l1
            compileStm stm1
            emit $ X86.Goto l2
            emit $ X86.Raw $ "L" ++ show l1 ++ ":"
            compileStm stm2
            emit $ X86.Raw $ "L" ++ show l2 ++ ":"
            resetTempReg
        (While expr@(ETyped e' t) stm) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            emit $ X86.Raw $ "L" ++ show l1 ++ ":"
            expr' <- compileExp expr 
            let b1 = isMemoryVar expr'
            case b1 of
                True -> emit $ X86.Compare2 (typeToItype t) expr' (X86.VInt 0)
                False -> emit $ X86.Compare expr' (X86.VInt 0)
            emit $ X86.CondB (X86.VVal "je") l2
            compileStm stm
            emit $ X86.Goto l1
            emit $ X86.Raw $ "L" ++ show l2 ++ ":"
            resetTempReg
        (SExp expr) -> do
            compileExp expr
            resetTempReg
            return ()

-- Compiles a expression
compileExp :: Expr -> CodeGen X86.Val
compileExp (ETyped (ELitTrue) t) = do
    r <- getNextTempReg t
    emit $ X86.Move r (X86.VInt 1)
    return $ r
compileExp (ETyped (ELitFalse) t) = do
    r <- getNextTempReg t
    emit $ X86.Move r (X86.VInt 0)
    return $ r
compileExp (ETyped (ELitInt i) t) = do
    r <- getNextTempReg t
    emit $ X86.Move r (X86.VInt i)
    return $ r
compileExp (ETyped (ELitDoub d) t) = do
    r <- getNextTempReg t
    (X86.VVal nameS) <- addGlobalData t (show d)
    emit $ X86.Fld (X86.VVal ("["++ nameS++"]"))
    return $ r
compileExp (ETyped (EVar id) t) = do
    (v,_)<- lookupVar id
    case t of 
        Doub -> do
            emit $ X86.Fld v
            r <- getNextTempReg t
            return r
        _      -> do
            r <- getNextTempReg t
            emit $ X86.Move r v
            return r
compileExp (ETyped (EApp id'@(Ident id) exps) t) = do
    expr' <- mapM compileExp exps
    mapM (\(x,ETyped e t) -> emit $ X86.Push x) (zip (reverse expr') (reverse exps))
    emit $ X86.Invoke id
    mapM (\x -> emit $ X86.Add (X86.VVal "esp") (X86.VInt 4)) (reverse expr')
    return $ X86.VVal "eax"
compileExp (ETyped (EString s) t) = do
    nameS <- addGlobalData t s
    emit $ X86.Cld
    return $ nameS
compileExp (ETyped (Neg e) t) = do
    e' <- compileExp e
    case t of 
        Doub -> do
            emit $ X86.Fxch e'
            emit $ X86.FNeg
            emit $ X86.Fxch e'
            return $ e'
        _    -> do
            r <- getNextTempReg t
            emit $ X86.Move r e'
            emit $ X86.Neg r
            return r
compileExp (ETyped (Not e) t) = do
    e' <- compileExp e
    r <- getNextTempReg t
    emit $ X86.Move r e'
    emit $ X86.Not r
    return $ r
compileExp (ETyped (EMul e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of
        Times -> do            
           
            case t of
                Doub -> do
                    emit $ X86.Fxch e1'
                    emit $ X86.FMul e2'
                    emit $ X86.Fxch e1'
                    return e1'
                _    -> do
                    r1 <- getNextTempReg t
                    r2 <- getNextTempReg t
                    emit $ X86.Move r1 e1'
                    emit $ X86.Move r2 e2'
                    emit $ X86.Mul r1 r2
                    return r1
        Div   -> do
            case t of 
                Doub -> do
                    doFDiv e1' e2'
                    return e1'
                _    -> do
                    doDiv e1' e2'        
                    return $ (X86.VVal "eax")
        Mod   -> do
            doDiv e1' e2'
            return $ (X86.VVal "edx")     
    where
        doDiv e1' e2' = do
            emit $ X86.Move (X86.VVal "edx") (X86.VInt 0)
            emit $ X86.Move2 (typeToItype t) (X86.VVal "eax") e1'
            emit $ X86.Div e2'
            {-case e2' of
                (X86.VVal _) -> do
                    case (isMemoryVar e2') of
                        True -> emit $ X86.Div2 (typeToItype t) e2'
                        False -> emit $ X86.Div e2'
                _ -> do
                    emit $ X86.Move (X86.VVal "ebx") e2'
                    emit $ X86.Div (X86.VVal "ebx")  -}
        doFDiv e1' e2' = do
            emit $ X86.Fxch e1'
            emit $ X86.FDiv e2'
            emit $ X86.Fxch e1'
compileExp (ETyped (EAdd e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    r <- getNextTempReg t
    --let b1 = isMemoryVar e1'
    --emit $ X86.Move (X86.VVal "eax") e1'
    case o of 
        Plus  -> do
            case t of 
                Doub -> do
                    emit $ X86.Fxch e1'
                    emit $ X86.FAdd e2'
                    emit $ X86.Fxch e1'
                    return e1' 
                _    -> do
                    emit $ X86.Move r e1'
                    emit $ X86.Add r e2'
                    return r
        Minus -> do
            case t of 
                Doub -> do
                    emit $ X86.Fxch e1'
                    emit $ X86.FSub e2'
                    emit $ X86.Fxch e1'
                    return e1' 
                _    -> do
                    emit $ X86.Move r e1'
                    emit $ X86.Sub r e2'
                    return r
compileExp (ETyped (ERel e1@(ETyped e1' t) o e2) t') = do
    l1 <- getNextLabel
    l2 <- getNextLabel
    e1' <- compileExp e1
    e2' <- compileExp e2
    r <- getNextTempReg t'
    let b1 = isMemoryVar e1'
    let b2 = isMemoryVar e2'
    case t of
        Doub -> do
            emit $ X86.Fxch e1'
            emit $ X86.FCompare e2'
            emit $ X86.Fxch e1'
        _ ->  case (b1 || b2) of
                True -> emit $ X86.Compare2 (typeToItype t) e1' e2'
                False -> emit $ X86.Compare e1' e2'
    case o of 
        LTH -> emit $ X86.CondB (X86.VVal "jl") l1
        LE  -> emit $ X86.CondB (X86.VVal "jle") l1 --return $ X86.VVal "jg"
        GTH -> emit $ X86.CondB (X86.VVal "jg") l1 --return $ X86.VVal "jle"
        GE  -> emit $ X86.CondB (X86.VVal "jge") l1 --return $ X86.VVal "jl"
        EQU -> emit $ X86.CondB (X86.VVal "je") l1 --return $ X86.VVal "jne"
        NE  -> emit $ X86.CondB (X86.VVal "jne") l1 --return $ X86.VVal "je"
    emit $ X86.Move r (X86.VInt 0)  
    emit $ X86.Goto l2
    emit $ X86.Raw $ "L" ++ show l1 ++ ":"
    emit $ X86.Move r (X86.VInt 1)  
    emit $ X86.Raw $ "L" ++ show l2 ++ ":"
    return r
compileExp (ETyped (EAnd e1 e2) t) = do
    l1 <- getNextLabel
    l2 <- getNextLabel
    l3 <- getNextLabel
    e1' <- compileExp e1
    r <- getNextTempReg t
    emit $ X86.And e1' (X86.VInt 1)
    emit $ X86.CondB (X86.VVal "je") l2
    e2' <- compileExp e2
    emit $ X86.And e1' e2'
    emit $ X86.CondB (X86.VVal "je") l2 
    emit $ X86.Raw $ "L" ++ show l1 ++ ":"
    emit $ X86.Move r (X86.VInt 0)
    emit $ X86.Goto l3
    emit $ X86.Raw $ "L" ++ show l2 ++ ":"
    emit $ X86.Move r (X86.VInt 1)
    emit $ X86.Raw $ "L" ++ show l3 ++ ":"
    return r
compileExp (ETyped (EOr e1 e2) t) = do
    l1 <- getNextLabel
    l2 <- getNextLabel
    l3 <- getNextLabel
    e1' <- compileExp e1
    r <- getNextTempReg t
    emit $ X86.And e1' (X86.VInt 0)
    emit $ X86.CondB (X86.VVal "je") l2 
    e2' <- compileExp e2
    emit $ X86.Or e1' e2'
    emit $ X86.CondB (X86.VVal "je") l2 
    emit $ X86.Raw $ "L" ++ show l1 ++ ":"
    emit $ X86.Move r (X86.VInt 0)
    emit $ X86.Goto l3
    emit $ X86.Raw $ "L" ++ show l2 ++ ":"
    emit $ X86.Move r (X86.VInt 1)
    emit $ X86.Raw $ "L" ++ show l3 ++ ":"
    return r
compileExp a = fail $ printTree a


-- * Helps functions for the code generator.

isMemoryVar :: X86.Val -> Bool
isMemoryVar v = case v of
    (X86.VVal s) -> do 
        case (s !! 0) == '[' of
            True -> True
            False -> False
    _ -> False  

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
allocateArgsNr :: [Arg] -> Int -> CodeGen ()
allocateArgsNr [] i = return ()
allocateArgsNr [Arg t id] i = extendContextvVal id t (X86.VVal ("[ebp+"++ show ((typeToNrBytes t) + i))) 
allocateArgsNr ((Arg t id):args) i = do
    extendContextvVal id t (X86.VVal ("[ebp+"++ show ((typeToNrBytes t) + i)))
    allocateArgsNr args (i+typeToNrBytes t)
 
allocateArgs :: [Arg] -> CodeGen ()
allocateArgs [] = return ()
allocateArgs [Arg t id] = extendContextvVal id t (X86.VVal "[ebp+8]")
allocateArgs ((Arg t id):args) = do
    extendContextvVal id t (X86.VVal "[ebp+8]")
    allocateArgsNr args 8
   

-- Contverts from Type to X86 types.
typeToItype :: Type -> X86.Size
typeToItype Int  = X86.Word
typeToItype Doub = X86.DWord
typeToItype Bool = X86.Bit
typeToItype Void = X86.Void
typeToItype (ArrayT t _) = typeToItype t

typeToNrBytes :: Type -> Int
typeToNrBytes Int = 4
typeToNrBytes Doub = 8
typeToNrBytes Bool = 1

        
-- Helps declar in the function "compileStm" to decide if variable is Initials or not 
declHelper :: Item -> Type -> CodeGen ()
declHelper (NoInit id) t = do
    sP <- gets stackP
    extendContextvVal id t (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]"))
    emit $ X86.Sub (X86.VVal "esp") (X86.VVal (show (typeToNrBytes t)))
    case t of 
        Doub -> do
            emit $ X86.Fldz 
            emit $ X86.Fst (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]"))
        _    -> emit $ X86.Move2 (typeToItype t) (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]")) (X86.VInt 0)
    incStackPointer (typeToNrBytes t)
declHelper (Init id expr) t = do
    e <- compileExp expr
    sP <- gets stackP
    extendContextvVal id t (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]"))
    emit $ X86.Sub (X86.VVal "esp") (X86.VVal (show (typeToNrBytes t)))
    case t of 
        Doub -> do
            emit $ X86.Fxch e
            emit $ X86.Fst (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]"))
            emit $ X86.Fxch e 
        _    -> emit $ X86.Move2 (typeToItype t) (X86.VVal ("[ebp-" ++ (show ((typeToNrBytes t)+sP)) ++ "]")) e
    incStackPointer (typeToNrBytes t)


    
    
    

   
