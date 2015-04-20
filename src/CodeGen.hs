module CodeGen where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import AbsJavalette
import PrintJavalette
import LexJavalette
import ParJavalette

import qualified LLVM

-- | Environment (handled by state monad)
data Env = Env
  { className  :: String
  , envSig     :: Sig
  , contexts   :: Contexts
  , code       :: [LLVM.Instruction]
  , label      :: LLVM.Label
  , tempReg    :: Int
  }

type Contexts = [VarContext]

-- | Variables in scope (symbol table)
data VarContext = VarContext
  { vars :: Map Ident (LLVM.Val,Type)
  , next :: LLVM.Val
  }

type Sig = Map Ident TopDef
  
type CodeGen = State Env


-- * Service functions

emit :: LLVM.Instruction -> CodeGen ()
emit i = modify $ updateCode $ (i :)

comment :: String -> CodeGen ()
comment = emit . LLVM.Comment

raw :: String -> CodeGen ()
raw = emit . LLVM.Raw

blank :: CodeGen ()
blank = raw ""

newBlock :: CodeGen ()
newBlock = do
    cont <- gets contexts
    case cont of
        [] -> modify $ updateContexts $ (emptyContext :)
        _ ->do
            let c@VarContext{next=(LLVM.VInt n)} = last cont
            modify $ updateContexts $ (almostemptyContext (n) :)

exitBlock :: CodeGen ()
exitBlock = do  
    modify $ updateContexts $ tail

extendContext :: Ident -> Type -> CodeGen ()
extendContext x t = modify $ updateContexts $ \ (b : bs) ->
  b { vars = Map.insert x (next b,t) (vars b)
    , next = (valAdd (next b) 1)  -- TODO double
    } : bs
    
valAdd :: LLVM.Val -> Integer -> LLVM.Val
valAdd (LLVM.VInt i) i1 = LLVM.VInt (i+i1)

extendEnvSig :: TopDef -> CodeGen () 
extendEnvSig def@(FnDef _t i _args _ss) = modify $ updateEnvSig i def
    
updateEnvSig :: Ident -> TopDef -> Env -> Env 
updateEnvSig i def env = env {envSig = Map.insert i def (envSig env)}

lookupVar :: Ident -> CodeGen (LLVM.Val,Type)
lookupVar x = do
  bs <- gets contexts
  case catMaybes $ map (Map.lookup x . vars) bs of
    []      -> error $ "unbound var " ++ printTree x
    (a : _) -> return a
    
lookDef :: Ident -> CodeGen TopDef
lookDef i = do
    env <- gets envSig
    case Map.lookup i ( env) of
        Nothing -> error $ "unbound func " ++ printTree i
        Just a  -> return a

    
getNextLabel :: CodeGen LLVM.Label
getNextLabel = do
    l <- gets label
    modify $ updateLabel (+1) 
    return l 

getNextTempReg :: CodeGen LLVM.Val
getNextTempReg = do
    r <- gets tempReg
    modify $ updateTempReg (+1)
    return (LLVM.VVal ("%t"++ show r)) 
    
getVarReg :: Ident -> CodeGen LLVM.Val
getVarReg id = do
    (r,_) <- lookupVar id
    return (LLVM.VVal ("%r"++show r))


getClassName :: CodeGen String
getClassName = do
    l <- gets className
    return l 
   
removeNL :: String -> String
removeNL s = filter (\a -> a/='\n') s
   
-- * Environment

emptyEnv :: Env
emptyEnv = Env
  { className  = ""
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  , tempReg    = 0
  }

-- TODO (TÅDÅ) maybe remove

almoastEmptyEnv :: String -> Env
almoastEmptyEnv s = Env {className = s
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  , tempReg    = 0
  }
  
updateContexts :: (Contexts -> Contexts) -> Env -> Env
updateContexts f env = env { contexts = f (contexts env) }

updateCode :: ([LLVM.Instruction] -> [LLVM.Instruction]) -> Env -> Env
updateCode f env = env { code = f (code env) }

updateLabel :: (LLVM.Label -> LLVM.Label) -> Env -> Env
updateLabel f env = env { label = f ( label env)}

updateTempReg :: (Int -> Int) -> Env -> Env
updateTempReg f env = env { tempReg = f ( tempReg env)}

-- * Contexts

emptyContext :: VarContext
emptyContext = VarContext
  { vars = Map.empty
  , next = LLVM.VInt 0
  }

-- TODO (TÅDÅ) maybe remove

almostemptyContext :: Integer -> VarContext
almostemptyContext i = VarContext
  { vars = Map.empty
  , next = LLVM.VInt i
  }


-- Code Generator

codeGen :: FilePath -> Program -> String
codeGen filename prg = header ++ unlines (map LLVM.showInstruction lcode)
    where
        lcode = reverse $ code $ compileProgram prg `execState` emptyEnv
        header = unlines ["declare void @printInt(i32)",""]

compileProgram :: Program -> CodeGen ()
compileProgram (Program defs) = do
    mapM_ extendEnvSig defs
    mapM_ compileDef defs

compileDef :: TopDef -> CodeGen ()
compileDef (FnDef t id'@(Ident id) args b@(Block ss)) = do
    newBlock     
    args' <- showA args
    emit $ LLVM.Raw $ "define " ++ LLVM.showSize (typeToItype t) ++ " @"++ id ++ "(" ++ args' ++ ") {"
    allocateArgs args
    case t of
         Void -> do
            let b = Block (ss ++ [VRet])
            compileBlock b
            exitBlock
            emit $ LLVM.Raw $ "}"
         _    -> do 
            compileBlock b
            let ssl = last ss
            case ssl of
                Cond _ _        -> emit $ LLVM.Raw $ "unreachable"
                CondElse _ _ _  -> emit $ LLVM.Raw $ "unreachable" 
                While _ _       -> emit $ LLVM.Raw $ "unreachable" 
                _               -> blank 
            exitBlock
            emit $ LLVM.Raw $ "}"
            

showA :: [Arg] -> CodeGen String
showA []           = return ""
showA ([Arg t id]) = do
    extendContext id t
    --r <- getVarReg id
    return $ LLVM.showSize (typeToItype t) ++ " %" ++ printTree id
showA ((Arg t id):ids) = do
    extendContext id t
    --r <- getVarReg id
    temp <- showA ids
    return $ LLVM.showSize (typeToItype t) ++ " %" ++ printTree id ++ " , " ++ temp

showE :: [Type] -> [LLVM.Val] ->  String
showE _ []              = "" 
showE [t] ([LLVM.VVal s]) = (LLVM.showSize (typeToItype t)) ++ " " ++ s
showE [t] ([LLVM.VInt i]) = (LLVM.showSize (typeToItype t)) ++ " " ++ show i
showE [t] ([LLVM.VDoub d]) = (LLVM.showSize (typeToItype t)) ++ " " ++ show d
showE (t:ts) ((LLVM.VVal s):ss) = (LLVM.showSize (typeToItype t)) ++ " " ++ s ++ " , " ++ showE ts ss
showE (t:ts) ((LLVM.VInt i):is) = (LLVM.showSize (typeToItype t)) ++ " " ++ show i ++ " , " ++ showE ts is
showE (t:ts) ((LLVM.VDoub d):ds) = (LLVM.showSize (typeToItype t)) ++ " " ++ show d ++ " , " ++ showE ts ds

allocateArgs :: [Arg] -> CodeGen ()
allocateArgs [] = return ()
allocateArgs [Arg t id] = do
    r <- getVarReg id
    emit $ LLVM.Raw $ (show r) ++ " = " ++ (LLVM.showInstruction $ LLVM.Alloca (typeToItype t))
    emit $ LLVM.Store (typeToItype t) (LLVM.VVal ("%"++printTree id)) (typeToItype t) r
allocateArgs ((Arg t id):args) = do
    r <- getVarReg id
    emit $ LLVM.Raw $ (show r) ++ " = " ++ (LLVM.showInstruction $ LLVM.Alloca (typeToItype t))
    emit $ LLVM.Store (typeToItype t) (LLVM.VVal ("%"++printTree id)) (typeToItype t) r
    allocateArgs args

typeToItype :: Type -> LLVM.Size
typeToItype Int  = LLVM.Word
typeToItype Doub = LLVM.DWord
typeToItype Bool = LLVM.Bit
typeToItype Void = LLVM.Void


compileBlock :: Block -> CodeGen ()
compileBlock (Block ss) = do
    --emit $ LLVM.Raw "{"    
    mapM_ compileStm ss
    --emit $ LLVM.Raw "}"

-- Return next free reg id
getNextReg :: Ident -> Type -> CodeGen (LLVM.Val)
getNextReg id t = do 
    extendContext id t
    (r,_) <- lookupVar id
    return r

compileStm :: Stmt -> CodeGen ()
compileStm s = do
    --blank
    --comment $ removeNL $ printTree s
    case s of
        Empty -> do
            blank   
        (BStmt b) -> do
            newBlock 
            compileBlock b
            exitBlock
        (Decl t i) -> do
            for i
            where for (item:[]) = declHelper item t
                  for (item:items)= declHelper item t >> for items
        (Ass id expr@(ETyped e t)) -> do
            r <- getVarReg id
            e' <- compileExp expr
            emit $ LLVM.Store (typeToItype t) e' (typeToItype t) r
        (Incr id) -> do
            (_,t) <- lookupVar id
            r <- getVarReg id
            (LLVM.VVal r1) <- getNextReg id t
            emit $ LLVM.Raw $ "%" ++ r1 ++ " = " ++ (LLVM.showInstruction $ LLVM.Load (typeToItype t) r) -- Load id into reg 
            (LLVM.VVal r2) <- getNextReg id t
            emit $ LLVM.Raw $ "%" ++ r2 ++ " = " ++ (LLVM.showInstruction $ LLVM.Add (typeToItype t) (LLVM.VVal r1) (LLVM.VInt 1)) -- Inc by one
            emit $ LLVM.Store (typeToItype t) (LLVM.VVal r2) (typeToItype t) r -- Store the inc value into id
        (Decr id) -> do
            (_,t) <- lookupVar id
            r <- getVarReg id
            (LLVM.VVal r1) <- getNextReg id t
            emit $ LLVM.Raw $ "%" ++ r1 ++ " = " ++ (LLVM.showInstruction $ LLVM.Load (typeToItype t) r) -- Load id into reg 
            (LLVM.VVal r2) <- getNextReg id t
            emit $ LLVM.Raw $ "%" ++ r2 ++ " = " ++ (LLVM.showInstruction $ LLVM.Sub  (typeToItype t) (LLVM.VVal r1) (LLVM.VInt 1)) -- Dec by one
            emit $ LLVM.Store (typeToItype t) (LLVM.VVal r2) (typeToItype t) r -- Store the dec value into id
        (Ret expr@(ETyped e t)) -> do
            e' <-  (compileExp expr)
            emit $ LLVM.Return (typeToItype t) e'
        (VRet) -> do
            emit $ LLVM.VReturn
        (Cond expr@(ETyped e' t) stm) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            r <- getNextTempReg
            condHelper expr r
            emit $ LLVM.CondB r l1 l2 -- Jump check
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label to enter if-statment
            compileStm stm
            emit $ LLVM.Goto l2
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label outside if-statment
        (CondElse expr@(ETyped e' t) stm1 stm2) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            l3 <- getNextLabel
            r <- getNextTempReg
            condHelper expr r
            emit $ LLVM.CondB r l1 l2 -- Jump check
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label to enter first stm
            compileStm stm1
            emit $ LLVM.Goto l3
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label to enter snd stm
            compileStm stm2
            emit $ LLVM.Goto l3
            emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out side if-statment
        (While expr@(ETyped e' t) stm) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            l3 <- getNextLabel
            r  <- getNextTempReg
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label at top
            condHelper expr r
            emit $ LLVM.CondB r l2 l3  
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label after condition 
            compileStm stm
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out of while          
        (SExp expr) -> do
            e <- compileExp expr
            case e of
                (LLVM.VVal a) -> emit $ LLVM.Raw a
                _ -> undefined
        
-- Helps declar to decide if variable is Init or not 
declHelper :: Item -> Type -> CodeGen ()
declHelper (NoInit id) t = do
    extendContext id t
    r <- getVarReg id
    emit $ LLVM.Raw $ (show r) ++ " = " ++ (LLVM.showInstruction $ LLVM.Alloca (typeToItype t))
declHelper (Init id expr) t = do 
    extendContext id t
    r <- getVarReg id
    emit $ LLVM.Raw $ (show r) ++ " = " ++ (LLVM.showInstruction $ LLVM.Alloca (typeToItype t))
    e' <- (compileExp expr)
    emit $ LLVM.Store (typeToItype t) e' (typeToItype t) r

-- Show the correct format for a register from Ident    
showReg :: Ident -> LLVM.Val
showReg i = LLVM.VVal (printTree i)

-- Helps the condition statmenst to emit right string
condHelper :: Expr -> LLVM.Val -> CodeGen ()
condHelper expr@(ETyped e t) r = case e of
    ELitTrue     -> do
       condEmiter expr r
    ELitFalse    -> do
       condEmiter expr r
    ELitInt i    -> do 
       condEmiter expr r
    ELitDoub d   -> do 
        condEmiter expr r
    EVar id      -> do
        condEmiter expr r
    ERel e1 o e2 -> do
        expr' <- compileExp expr
        emit $ LLVM.Ass r expr'
    _            -> undefined

-- Adds zero with a value into a register
condEmiter :: Expr -> LLVM.Val -> CodeGen ()
condEmiter expr@(ETyped e' t) (LLVM.VVal r) = do
    e <- compileExp expr 
    emit $ LLVM.Raw $ r ++ " = " ++ (LLVM.showInstruction $ LLVM.Add (typeToItype t) e (LLVM.VInt 0))


compileExp :: Expr -> CodeGen LLVM.Val
compileExp (ETyped (ELitTrue) t) = return $ LLVM.VInt 1
compileExp (ETyped (ELitFalse) t) = return $ LLVM.VInt 0
compileExp (ETyped (ELitInt i) t) = return $ LLVM.VInt i
compileExp (ETyped (ELitDoub d) t) = return $ LLVM.VDoub d
compileExp (ETyped (EVar id) t) = do
    r1 <- getNextTempReg
    r <- getVarReg id
    emit $ LLVM.Raw $ (show r1) ++ " = " ++ (LLVM.showInstruction $ LLVM.Load (typeToItype t) r)  
    return $ r1
compileExp (ETyped (EApp id'@(Ident id) exps) t) = do
    par' <- sequence $ map compileExp exps
    let types = map (\ (ETyped e t) -> t ) exps     
    let par = showE types par' 
    let f = ("@"++ id ++ "(" ++ par ++ ")")
    case t of
        Void -> do
            return $ LLVM.VVal (LLVM.showInstruction $ LLVM.Invoke (typeToItype t) f)
        _    -> do
            r <- getNextTempReg
            emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Invoke (typeToItype t) f)) 
            return r


--compileExp (VVal r) (ETyped (EString s) t) =return ""
compileExp (ETyped (Neg e) t) = do
    e' <- compileExp e
    r <- getNextTempReg 
    emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Neg (typeToItype t) e'))
    return r 
--compileExp (VVal r) (ETyped (Not e) t) =  return ""
compileExp (ETyped (EMul e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of
        Times -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Mul (typeToItype t) e1' e2')) 
            return r
        Div -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Div (typeToItype t) e1' e2')) 
            return r
        Mod -> undefined -- TODO Add later
compileExp (ETyped (EAdd e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of 
        Plus  -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Add (typeToItype t) e1' e2')) 
            return r
        Minus -> do 
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.VVal (LLVM.showInstruction $ LLVM.Sub (typeToItype t) e1' e2')) 
            return r
compileExp (ETyped (ERel e1@(ETyped e1' t) o e2) t') = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of 
        LTH -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Slt (typeToItype t) e1' e2' 
        LE  -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Sle (typeToItype t) e1' e2' --TODO prob wrong
        GTH -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Sgt (typeToItype t) e1' e2' 
        GE  -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Sge (typeToItype t) e1' e2' 
        EQU -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Equ (typeToItype t) e1' e2' 
        NE  -> return $ LLVM.VVal $ LLVM.showInstruction $ LLVM.Compare LLVM.Ne  (typeToItype t) e1' e2' 
--compileExp (VVal r) (ETyped (EAnd e1 e2) t) =return ""
--compileExp (VVal r) (ETyped (EOr e1 e2) t) =return ""
compileExp a =  fail $ printTree a 
            



 





