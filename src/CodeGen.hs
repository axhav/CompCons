module CodeGen where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Char

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
  , globalList :: [LLVM.Instruction]
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

-- Adds the input instruction to generated code 
emit :: LLVM.Instruction -> CodeGen ()
emit i = modify $ updateCode $ (i :)

-- Adds a comment to the generated code
comment :: String -> CodeGen ()
comment = emit . LLVM.Comment

-- Adds a newline to the generated code
blank :: CodeGen ()
blank = emit $ LLVM.Raw ""

-- Creates a new block.
newBlock :: CodeGen ()
newBlock = do
    cont <- gets contexts
    case cont of
        [] -> modify $ updateContexts $ (emptyContext :)
        _ ->do
            let c@VarContext{next=(LLVM.VInt n)} = head cont
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
    
extendContextvVal :: Ident -> Type -> LLVM.Val -> CodeGen ()
extendContextvVal id t v = modify $ updateContexts $ \ (b : bs) ->
  b { vars = Map.insert id (v,t) (vars b)
    } : bs

-- Increment reg/label counter.    
valAdd :: LLVM.Val -> Integer -> LLVM.Val
valAdd (LLVM.VInt i) i1 = LLVM.VInt (i+i1)

extendEnvSig :: TopDef -> CodeGen () 
extendEnvSig def@(FnDef _t i _args _ss) = modify $ updateEnvSig i def

-- Looks up the input variable and return the Val and type of variable.
lookupVar :: Ident -> CodeGen (LLVM.Val,Type)
lookupVar x = do
  bs <- gets contexts
  case catMaybes $ map (Map.lookup x . vars) bs of
    []      -> error $ "unbound var " ++ printTree x
    (a : _) -> return a

-- Returns the next label number.    
getNextLabel :: CodeGen LLVM.Label
getNextLabel = do
    l <- gets label
    modify $ updateLabel (+1) 
    return l 

-- Return the next temp register number.
getNextTempReg :: CodeGen LLVM.Val
getNextTempReg = do
    r <- gets tempReg
    modify $ updateTempReg (+1)
    return (LLVM.VVal ("%t"++ show r)) 

-- Returns the register number for a variable.    
getVarReg :: Ident -> CodeGen LLVM.Val
getVarReg id = do
    (r,_) <- lookupVar id
    case head (show r) of
        '%' -> return (LLVM.VVal (show r))  
        _ -> return (LLVM.VVal ("%r"++show r))

-- Insert new global variable into globalList and return the variable name.
setNextGlobalVar :: String -> CodeGen LLVM.Val
setNextGlobalVar s = do
    gl <- gets globalList
    let gName = LLVM.VVal $ "@G" ++ (show $ length gl)
    let l = (length s) + 1
    modify $ updateGlobalList ((LLVM.GString gName l (LLVM.VVal s)):)
    return gName

-- Insert new global variable into globalList and return the variable name. TODO
setNextGlobalArr :: Type -> Bracket -> CodeGen String
setNextGlobalArr t b = do
    gl <- gets globalList
    case b of
        (Brackets e b') -> do
            setNextGlobalArr t b'
            ((LLVM.GStruct _ gl1):gls) <- gets globalList
            let typeName = takeWhile (\x -> not (isNumber x) ) gl1
            let index = sum [ y | y <- zipWith (*) (reverse (map digitToInt (filter isNumber (takeWhile (/=' ') gl1)))) [1,10..]]
            let gName = typeName ++ show (index+1) --TODO MAYBE not works for doub and bool?
            unless ((filter (\ (LLVM.GStruct _ a) -> a == gName) gl) /= [] ) $
                modify $ updateGlobalList ((LLVM.GStruct (LLVM.SSize ((typeToArrT t)++show index)) gName):)
            return gName 
        (NoBracket e) -> do
            case t of
                Int  -> do
                    let gName = "%arrInt0" 
                    unless ((filter (\ (LLVM.GStruct _ a) -> a == gName) gl) /= [] ) $ 
                        modify $ updateGlobalList ((LLVM.GStruct (typeToItype Int) gName):)
                    return gName 
                Doub -> do
                    let gName = "%arrDoub0"
                    unless ((filter (\ (LLVM.GStruct _ a) -> a == gName) gl) /= [] ) $  
                        modify $ updateGlobalList ((LLVM.GStruct (typeToItype Doub) gName):)
                    return gName
                Bool -> do
                    let gName = "%arrBool0"
                    unless ((filter (\ (LLVM.GStruct _ a) -> a == gName) gl) /= [] ) $ 
                        modify $ updateGlobalList ((LLVM.GStruct (typeToItype Bool) gName):)
                    return gName
    
 
-- * Environment

emptyEnv :: Env
emptyEnv = Env
  { className  = ""
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  , tempReg    = 0
  , globalList = []
  }

updateEnvSig :: Ident -> TopDef -> Env -> Env 
updateEnvSig i def env = env {envSig = Map.insert i def (envSig env)}
  
updateContexts :: (Contexts -> Contexts) -> Env -> Env
updateContexts f env = env { contexts = f (contexts env) }

updateCode :: ([LLVM.Instruction] -> [LLVM.Instruction]) -> Env -> Env
updateCode f env = env { code = f (code env) }

updateLabel :: (LLVM.Label -> LLVM.Label) -> Env -> Env
updateLabel f env = env { label = f ( label env)}

updateTempReg :: (Int -> Int) -> Env -> Env
updateTempReg f env = env { tempReg = f ( tempReg env)}

updateGlobalList :: ([LLVM.Instruction] -> [LLVM.Instruction]) -> Env -> Env
updateGlobalList f env = env { globalList = f (globalList env) }

-- * Contexts

emptyContext :: VarContext
emptyContext = VarContext
  { vars = Map.empty
  , next = LLVM.VInt 0
  }

almostemptyContext :: Integer -> VarContext
almostemptyContext i = VarContext
  { vars = Map.empty
  , next = LLVM.VInt i
  }


-- * Code Generator

-- Generats the code and returns it as a string.
codeGen :: Program -> String
codeGen prg = header ++ unlines (map LLVM.showInstruction lcode)
    where
        compileCode = compileProgram prg `execState` emptyEnv
        lcode = reverse $ code compileCode ++ globalList compileCode
        header = unlines (["declare void @printInt(i32)","declare void @printString(i8*)", 
            "declare void @printDouble(double)", "declare i32 @readInt()", 
            "declare double @readDouble()", "declare i8* @calloc(i32, i32)", ""]) 

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
    emit $ LLVM.Raw $ "define " ++ LLVM.showSize (argTy t) ++ " @"++ id ++ "(" ++ args' ++ ") {"
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
                _     -> emit $ LLVM.Raw $ "unreachable"
    exitBlock
    emit $ LLVM.Raw $ "}"

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
            --fail $ printTree b
            compileBlock b
            exitBlock
        (Decl t i) -> do
            for i
            where for (item:[]) = declHelper item t
                  for (item:items)= declHelper item t >> for items
        (Ass e1 expr@(ETyped e t)) -> do
            case e1 of
                (ETyped (EVar id) _) -> do
                    r <- getVarReg id
                    e' <- compileExp expr
                    emit $ LLVM.Store (argTy t) e' (argTy t) r
                (ETyped (EIndex (ETyped (EVar id) _) b) t') -> do
                    (_,ArrayT t' b') <- lookupVar id
                    r <- getVarReg id
                    r1 <- compileBracket b' r t b
                    e4' <- compileExp expr
--                    r1 <- compileBracket b r t' b
                    case cmpBracketLenght b' b of
                        True -> emit $ LLVM.Store (typeToItype t) e4' (typeToItype t) r1 
                        _    -> do
                            let arrT = typeToArrT t
                            let arrS = show $ arrayFindDepth b' - arrayFindDepth b - 1
                            emit $ LLVM.Store (LLVM.SSize (arrT ++ arrS)) e4' (LLVM.SSize (arrT ++ arrS)) r1 
                _ -> do
                    fail $ printTree s
        (Incr id) -> do
            (_,t) <- lookupVar id
            r <- getVarReg id
            r1 <- getNextTempReg
            emit $ LLVM.Ass r1 (LLVM.Load (typeToItype t) r) -- Load id into reg 
            r2 <- getNextTempReg
            emit $ LLVM.Ass r2 (LLVM.Add (typeToItype t) r1 (LLVM.VInt 1)) -- Inc by one
            emit $ LLVM.Store (typeToItype t) r2 (typeToItype t) r -- Store the inc value into id
        (Decr id) -> do
            (_,t) <- lookupVar id
            r <- getVarReg id
            r1 <- getNextTempReg
            emit $ LLVM.Ass r1 (LLVM.Load (typeToItype t) r) -- Load id into reg 
            r2 <- getNextTempReg
            emit $ LLVM.Ass r2 (LLVM.Sub  (typeToItype t) r1 (LLVM.VInt 1)) -- Dec by one
            emit $ LLVM.Store (typeToItype t) r2 (typeToItype t) r -- Store the dec value into id
        (Ret expr@(ETyped e t)) -> do
            e' <-  (compileExp expr)
            emit $ LLVM.Return (argTy t) e'
        (VRet) -> do
            emit $ LLVM.VReturn
        (Cond expr@(ETyped e' t) stm) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            r <- compileExp expr
            emit $ LLVM.CondB r l1 l2 -- Jump check
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label to enter if-statment
            compileStm stm
            emit $ LLVM.Goto l2
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label outside if-statment
        (CondElse expr@(ETyped e' t) stm1 stm2) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            l3 <- getNextLabel
            r <- compileExp expr
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
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label at top
            r  <- compileExp expr
            emit $ LLVM.CondB r l2 l3  
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label after condition 
            compileStm stm
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out of while
        (ForEach t id e@(ETyped e' (ArrayT t1' b)) stm) -> do
            extendContext id t
            l1 <- getNextLabel
            l2 <- getNextLabel
            l3 <- getNextLabel
            r1 <- getNextTempReg
            r2 <- getNextTempReg
            r3 <- getVarReg id
            r4 <- getNextTempReg
            r5 <- getNextTempReg
            r6 <- getNextTempReg
            r7 <- getNextTempReg
            r8 <- getNextTempReg
            r9 <- getNextTempReg
            r10 <- getNextTempReg
            r11 <- getNextTempReg
            r <- compileExp e
            --fail $ printTree e 
            let bType = bracketToArrT b t1'
            let typeName = takeWhile (\x -> not (isNumber x) ) (LLVM.showSize bType)
            let j =  sum [ y | y <- zipWith (*) (reverse (map digitToInt (filter isNumber (takeWhile (/=' ') (LLVM.showSize bType))))) [1,10..]]
            bType' <- case j of
                0 -> return $ typeToItype t
                _ -> return $ LLVM.SSize (typeName ++ show (j-1))
             
            
            -- Init of variable for forEach loop           
            emit $ LLVM.Ass r1 (LLVM.GetElmPtr bType r 0 (LLVM.VInt 0)) -- arraySize
            emit $ LLVM.Ass r2 (LLVM.Load (typeToItype Int) r1)
            case bType' of 
                LLVM.SSize _->  blank
                _ -> emit $ LLVM.Ass r3 (LLVM.Alloca bType')
            
            emit $ LLVM.Ass r4 (LLVM.Alloca LLVM.Word)
            emit $ LLVM.Store LLVM.Word (LLVM.VInt 0) LLVM.Word r4   
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label at top
            emit $ LLVM.Ass r6 (LLVM.Load LLVM.Word r4)            
            emit $ LLVM.Ass r5 (LLVM.Compare LLVM.Eq LLVM.Word r2 r6)
            emit $ LLVM.CondB r5 l3 l2  
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label after condition 
            emit $ LLVM.Ass r7 (LLVM.GetElmPtr bType r 0 (LLVM.VInt 1)) -- arrayPointer
            emit $ LLVM.Ass r8 (LLVM.Load (LLVM.P (LLVM.A bType' 0)) r7)
            emit $ LLVM.Ass r9 (LLVM.GetElmPtr (LLVM.P (LLVM.A bType' 0)) r8 0 r6)
            case bType' of 
                LLVM.SSize _->  emit $ LLVM.Ass r3 (LLVM.Load bType' r9)
                _ -> do 
                    emit $ LLVM.Ass r10 (LLVM.Load bType' r9)
                    emit $ LLVM.Store bType' r10 bType' r3   
            compileStm stm
            emit $ LLVM.Ass r11 (LLVM.Add LLVM.Word r6 (LLVM.VInt 1))
            emit $ LLVM.Store LLVM.Word r11 LLVM.Word r4   
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out of forEach          
        (SExp expr) -> do
            e <- compileExp expr
            case e of
                (LLVM.VVal a) -> emit $ LLVM.Raw a
                _ -> undefined

-- Compiles a expression
compileExp :: Expr -> CodeGen LLVM.Val
compileExp (ETyped (ELitTrue) t) = return $ LLVM.VInt 1
compileExp (ETyped (ELitFalse) t) = return $ LLVM.VInt 0
compileExp (ETyped (ELitInt i) t) = return $ LLVM.VInt i
compileExp (ETyped (ELitDoub d) t) = return $ LLVM.VDoub d
compileExp (ETyped (EVar id) t) = case t of
    (ArrayT _ _) -> do
        r <- getVarReg id
        return $ r
    _ -> do
        r1 <- getNextTempReg
        r <- getVarReg id
        emit $ LLVM.Ass r1 (LLVM.Load (typeToItype t) r)   
        return $ r1
compileExp (ETyped (EApp id'@(Ident id) exps) t) = do
    par' <- sequence $ map compileExp exps
    let types = map (\ (ETyped e t) -> t ) exps     
    let par = showE types par' 
    let f = ("@"++ id ++ "(" ++ par ++ ")")
    case t of
        Void -> do
            return $ LLVM.VVal (LLVM.showInstruction $ LLVM.Invoke (argTy t) f)
        _    -> do
            r <- getNextTempReg
            emit $ LLVM.Ass r (LLVM.Invoke (argTy t) f) 
            return r
compileExp (ETyped (EString s) t) = do
    r1 <- setNextGlobalVar s
    r2 <- getNextTempReg
    let l = (length s) + 1
    emit $ LLVM.Ass r2 (LLVM.TwoArray l r1 0 0)
    return r2
compileExp (ETyped (EIndex e1@(ETyped (EVar id) _) b) t) = do
    (_,ArrayT t' b') <- lookupVar id
    r <- getVarReg id
    r1 <- compileBracket b' r t b
    r2 <- getNextTempReg
    case cmpBracketLenght b' b of
        True -> emit $ LLVM.Ass r2 (LLVM.Load (typeToItype t) r1)
        _    -> do
            let arrT = typeToArrT t
            let arrS = show $ arrayFindDepth b' - arrayFindDepth b - 1
            emit $ LLVM.Ass r2 (LLVM.Load (LLVM.SSize (arrT ++ arrS)) r1)
    return r2
compileExp (ETyped (EDot e1 e2) t) = do 
    r <- compileExp e1
    r1 <- getNextTempReg
    r2 <- getNextTempReg
    case e1 of
        (ETyped e (ArrayT t' b)) -> emit $ LLVM.Ass r1 (LLVM.GetElmPtr (bracketToArrT b t') r 0 (LLVM.VInt 0))
        _                    -> emit $ LLVM.Ass r1 (LLVM.GetElmPtr (bracketToArrT (NoBracket []) t) r 0 (LLVM.VInt 0))
    emit $ LLVM.Ass r2 (LLVM.Load (typeToItype Int) r1)
    return r2
compileExp (ETyped (Neg e) t) = do
    e' <- compileExp e
    r <- getNextTempReg 
    emit $ LLVM.Ass r (LLVM.Neg (typeToItype t) e')
    return r 
compileExp (ETyped (Not e) t) = do
    e' <- compileExp e
    r <- getNextTempReg
    emit $ LLVM.Ass r (LLVM.Not e')
    return r
compileExp (ETyped (EMul e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of
        Times -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.Mul (typeToItype t) e1' e2')
            return r
        Div   -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.Div (typeToItype t) e1' e2')
            return r
        Mod   -> do 
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.Mod (typeToItype t) e1' e2')
            return r
compileExp (ETyped (EAdd e1 o e2) t) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    case o of 
        Plus  -> do
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.Add (typeToItype t) e1' e2') 
            return r
        Minus -> do 
            r <- getNextTempReg 
            emit $ LLVM.Ass r (LLVM.Sub (typeToItype t) e1' e2')
            return r
compileExp (ETyped (ERel e1@(ETyped e1' t) o e2) t') = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    r <- getNextTempReg
    case o of 
        LTH -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Slt (typeToItype t) e1' e2') 
            return r
        LE  -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Sle (typeToItype t) e1' e2') 
            return r
        GTH -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Sgt (typeToItype t) e1' e2') 
            return r
        GE  -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Sge (typeToItype t) e1' e2') 
            return r
        EQU -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Eq (typeToItype t) e1' e2') 
            return r
        NE  -> do
            emit $ LLVM.Ass r (LLVM.Compare LLVM.Ne (typeToItype t) e1' e2') 
            return r
compileExp (ETyped (EAnd e1 e2) t) = do
    l1 <- getNextLabel
    l2 <- getNextLabel
    r1 <- getNextTempReg 
    emit $ LLVM.Ass r1 (LLVM.Alloca (typeToItype t))
    e1' <- compileExp e1
    emit $ LLVM.Store (typeToItype t) e1' (typeToItype t) r1
    emit $ LLVM.CondB e1' l1 l2
    emit $ LLVM.Raw $ "L" ++ show l1 ++ ":"  
    e2' <- compileExp e2
    r2 <- getNextTempReg
    emit $ LLVM.Ass r2 (LLVM.And (typeToItype t) e1' e2')
    emit $ LLVM.Store (typeToItype t) r2 (typeToItype t) r1
    emit $ LLVM.Goto l2
    emit $ LLVM.Raw $ "L" ++ show l2 ++ ":"    
    r3 <- getNextTempReg
    emit $ LLVM.Ass r3 (LLVM.Load (typeToItype t) r1)
    return r3
compileExp (ETyped (EOr e1 e2) t) = do
    l1 <- getNextLabel
    l2 <- getNextLabel
    r1 <- getNextTempReg 
    emit $ LLVM.Ass r1 (LLVM.Alloca (typeToItype t))
    e1' <- compileExp e1
    emit $ LLVM.Store (typeToItype t) e1' (typeToItype t) r1
    emit $ LLVM.CondB e1' l2 l1
    emit $ LLVM.Raw $ "L" ++ show l1 ++ ":"  
    e2' <- compileExp e2
    r2 <- getNextTempReg
    emit $ LLVM.Ass r2 (LLVM.Or (typeToItype t) e1' e2') 
    emit $ LLVM.Store (typeToItype t) r2 (typeToItype t) r1
    emit $ LLVM.Goto l2
    emit $ LLVM.Raw $ "L" ++ show l2 ++ ":"    
    r3 <- getNextTempReg
    emit $ LLVM.Ass r3 (LLVM.Load (typeToItype t) r1)
    return r3
compileExp (ETyped (EArr t1@(ArrayT t b)) t2) = do
    --fail $ printTree t1 ++ "    " ++ printTree t ++ "     "++ printTree t2
    g <- setNextGlobalArr t b
    r1 <- emitMultiArray b t2
    return r1 
compileExp a = fail $ printTree a

-- Compiles and finds the address for a index in a multi-arrays.
compileBracket :: Bracket -> LLVM.Val -> Type -> Bracket -> CodeGen LLVM.Val
compileBracket b r t b2 = do
        let bType = bracketToArrT b t
        let typeName = takeWhile (\x -> not (isNumber x) ) (LLVM.showSize bType)
        let j =  sum [ y | y <- zipWith (*) (reverse (map digitToInt (filter isNumber (takeWhile (/=' ') (LLVM.showSize bType))))) [1,10..]]
        let bType' = LLVM.SSize (typeName ++ show (j-1))
        let bType'' = LLVM.SSize (typeName ++ show (j+1))
        r1 <- getNextTempReg
        r2 <- getNextTempReg
        r3 <- getNextTempReg
        r4 <- getNextTempReg
        
        emit $ LLVM.Ass r1 (LLVM.GetElmPtr bType r 0 (LLVM.VInt 1))
        case b of 
            (NoBracket e) -> do
                e' <- compileExp (getExpFromBracket b2)
                emit $ LLVM.Ass r2 (LLVM.Load (LLVM.P (LLVM.A (typeToItype t) 0)) r1)
                emit $ LLVM.Ass r3 (LLVM.GetElmPtr (LLVM.P (LLVM.A (typeToItype t) 0)) r2 0 e')
                --emit $ LLVM.Ass r4 (LLVM.Load (typeToItype t) r3)
                return r3
            (Brackets e b') -> do
                e' <- compileExp (getExpFromBracket b2)
                emit $ LLVM.Ass r2 (LLVM.Load (LLVM.P (LLVM.A bType' 0)) r1)
                emit $ LLVM.Ass r3 (LLVM.GetElmPtr (LLVM.P (LLVM.A bType' 0)) r2 0 e') 
                
                ret2  <- case b2 of
                    NoBracket _ -> return r3
                    Brackets _ b3-> do 
                        emit $ LLVM.Ass r4 (LLVM.Load bType' r3)
                        ret <- compileBracket b' r4 t b3
                        return ret 
                return ret2

-- * Helps functions for the code generator.
-- A help function that declars a multi-dimension array.
emitMultiArray :: Bracket -> Type -> CodeGen LLVM.Val
emitMultiArray b t2 = case b of
    (Brackets (e@(ETyped e' t):es) b') -> do
        l1 <- getNextLabel
        l2 <- getNextLabel
        l3 <- getNextLabel
        r1 <- getNextTempReg
        r2 <- getNextTempReg        
        r3 <- getNextTempReg
        r4 <- getNextTempReg
        r5 <- getNextTempReg
        r6 <- getNextTempReg
        r7 <- getNextTempReg
        r8 <- getNextTempReg
        r9 <- getNextTempReg
        let bType = bracketToArrT b t2
        let typeName = takeWhile (\x -> not (isNumber x) ) (LLVM.showSize bType)
        let j =  sum [ y | y <- zipWith (*) (reverse (map digitToInt (filter isNumber (takeWhile (/=' ') (LLVM.showSize bType))))) [1,10..]]
        let bType' = LLVM.SSize (typeName ++ show (j-1))
        ret <- emitArray b t2     
        blank
        comment $ printTree b
        emit $ LLVM.Ass r1 (LLVM.GetElmPtr bType ret 0 (LLVM.VInt 0))
        emit $ LLVM.Ass r2 (LLVM.Load LLVM.Word r1)

        emit $ LLVM.Ass r3 (LLVM.Alloca LLVM.Word)
        emit $ LLVM.Store LLVM.Word (LLVM.VInt 0) LLVM.Word r3  
        emit $ LLVM.Goto l1
        emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label at top
        emit $ LLVM.Ass r5 (LLVM.Load LLVM.Word r3)            
        emit $ LLVM.Ass r4 (LLVM.Compare LLVM.Eq LLVM.Word r2 r5)
        emit $ LLVM.CondB r4 l3 l2   
        emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label after condition 
		
        blank
		
        ret2 <- emitMultiArray b' t2

        blank
		
        -- Store emit arrays into array above
        emit $ LLVM.Ass r7 (LLVM.GetElmPtr bType ret 0 (LLVM.VInt 1))
        emit $ LLVM.Ass r8 (LLVM.Load  (LLVM.P (LLVM.A bType' 0)) r7)
        emit $ LLVM.Ass r9 (LLVM.GetElmPtr (LLVM.P (LLVM.A bType' 0)) r8 0 r5)
        emit $ LLVM.Store bType' ret2 bType' r9  
        
        emit $ LLVM.Ass r6 (LLVM.Add LLVM.Word r5 (LLVM.VInt 1))
        emit $ LLVM.Store LLVM.Word r6 LLVM.Word r3   
        emit $ LLVM.Goto l1
        emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out of while
        return ret   
    (NoBracket (e@(ETyped e' t):es)) -> do
        ret <- emitArray b t2
        return ret
        
-- Declare a array with the passed Bracket and Type  
emitArray :: Bracket -> Type -> CodeGen LLVM.Val
emitArray b t2 = do
    let bType = bracketToArrT b t2
    let typeName = takeWhile (\x -> not (isNumber x) ) (LLVM.showSize bType)
    let j =  sum [ y | y <- zipWith (*) (reverse (map digitToInt (filter isNumber (takeWhile (/=' ') (LLVM.showSize bType))))) [1,10..]]
    let bType' = LLVM.SSize (typeName ++ show (j-1))
    r1 <- getNextTempReg
    r2 <- getNextTempReg
    r3 <- getHardwareSizeOfType t2
    r5 <- getNextTempReg
    r6 <- getNextTempReg
    r7 <- getNextTempReg
    emit $ LLVM.Ass r1 (LLVM.Alloca (LLVM.SSize ((LLVM.showSize bType) ++ "Struct")))
    
    (e':is) <- case b of 
        (Brackets e t) -> mapM compileExp e
        (NoBracket e) -> mapM compileExp e
    
    let f = "@calloc(i32 " ++ show e' ++ ", i32 " ++ show r3 ++")"
    emit $ LLVM.Ass r2 (LLVM.Invoke (LLVM.P LLVM.Byte) f)
    case j > 0 of
        True  -> emit $ LLVM.Ass r5 (LLVM.BitCast (LLVM.P LLVM.Byte) r2 (LLVM.P $ LLVM.A bType' 0)) 
        False ->  emit $ LLVM.Ass r5 (LLVM.BitCast (LLVM.P LLVM.Byte) r2 (LLVM.P $ LLVM.A (typeToItype t2) 0)) 
        
    --Stores size of array to struct
    emit $ LLVM.Ass r6 (LLVM.GetElmPtr bType r1 0 (LLVM.VInt 0))
    emit $ LLVM.Store LLVM.Word e' LLVM.Word r6
    
    --Stores calloc pointer to struct
    emit $ LLVM.Ass r7 (LLVM.GetElmPtr bType r1 0 (LLVM.VInt 1))  
    case j > 0 of
        True  -> emit $ LLVM.Store (LLVM.P (LLVM.A bType' 0)) r5 (LLVM.P (LLVM.A bType' 0)) r7
        False -> emit $ LLVM.Store (LLVM.P (LLVM.A (typeToItype t2) 0)) r5 (LLVM.P (LLVM.A (typeToItype t2) 0)) r7
    return r1 

-- Returns the size of type inside a register.
getHardwareSizeOfType :: Type -> CodeGen LLVM.Val
getHardwareSizeOfType t = do
    r1 <- getNextTempReg
    r2 <- getNextTempReg
    emit $ LLVM.Ass r1 (LLVM.Raw $ "getelementptr " ++ (LLVM.showSize (LLVM.P (typeToItype t))) ++ " null, i32 1")
    emit $ LLVM.Ass r2 (LLVM.PtrToInt (typeToItype t) r1 LLVM.Word)
    return r2

-- Generats and returns the code for the arguments to a code block/method.
showA :: [Arg] -> CodeGen String
showA []           = return ""
showA ([Arg t id]) = do
    extendContext id t
    case t of
        (ArrayT t1 e1) -> do
            r <- getVarReg id
            return $ LLVM.showSize (argTy t) ++ " " ++ show r  
        _ -> do
            return $ LLVM.showSize (argTy t) ++ " %" ++ printTree id
showA ((Arg t id):ids) = do
    extendContext id t
    case t of
        (ArrayT t1 e1) -> do
            r <- getVarReg id
            temp <- showA ids
            return $ LLVM.showSize (argTy t) ++ " " ++ show r ++ " , " ++ temp
        _ -> do
            temp <- showA ids
            return $ LLVM.showSize (argTy t) ++ " %" ++ printTree id ++ " , " ++ temp

-- Generats and returns the code for the arguments/input variables for method calls
showE :: [Type] -> [LLVM.Val] ->  String
showE _ []              = "" 
showE [Void] ([LLVM.VVal s]) = "i8* " ++ s  
showE [t] ([LLVM.VVal s]) = (LLVM.showSize (argTy t)) ++ " " ++ s
showE [t] ([LLVM.VInt i]) = (LLVM.showSize (argTy t)) ++ " " ++ show i
showE [t] ([LLVM.VDoub d]) = (LLVM.showSize (argTy t)) ++ " " ++ show d
showE (Void:ts) ((LLVM.VVal s):ss) = "i8* " ++ s ++ showE ts ss
showE (t:ts) ((LLVM.VVal s):ss) = (LLVM.showSize (argTy t)) ++ " " ++ s ++ " , " ++ showE ts ss
showE (t:ts) ((LLVM.VInt i):is) = (LLVM.showSize (argTy t)) ++ " " ++ show i ++ " , " ++ showE ts is
showE (t:ts) ((LLVM.VDoub d):ds) = (LLVM.showSize (argTy t)) ++ " " ++ show d ++ " , " ++ showE ts ds

-- Saves/allocates the arguemnts register onto the stack.
allocateArgs :: [Arg] -> CodeGen ()
allocateArgs [] = return ()
allocateArgs [Arg t id] = case t of
    (ArrayT _ _) -> blank
    _ -> do
        r <- getVarReg id
        emit $ LLVM.Ass r (LLVM.Alloca (typeToItype t))
        emit $ LLVM.Store (typeToItype t) (LLVM.VVal ("%"++printTree id)) (typeToItype t) r
allocateArgs ((Arg t id):args) = case t of
    (ArrayT _ _) -> allocateArgs args
    _ -> do
        r <- getVarReg id
        emit $ LLVM.Ass r (LLVM.Alloca (typeToItype t))
        emit $ LLVM.Store (typeToItype t) (LLVM.VVal ("%"++printTree id)) (typeToItype t) r
        allocateArgs args

-- Contverts from Type to LLVM types.
typeToItype :: Type -> LLVM.Size
typeToItype Int  = LLVM.Word
typeToItype Doub = LLVM.DWord
typeToItype Bool = LLVM.Bit
typeToItype Void = LLVM.Void
typeToItype (ArrayT t _) = typeToItype t

typeToArrT :: Type -> String
typeToArrT Int = "%arrInt"
typeToArrT Doub = "%arrDoub"
typeToArrT Bool = "%arrBool"
typeToArrT (ArrayT t b) = typeToArrT t


getExpFromBracket :: Bracket -> Expr
getExpFromBracket (Brackets [e] b) = e
getExpFromBracket (NoBracket [e]) = e

cmpBracketLenght :: Bracket -> Bracket -> Bool
cmpBracketLenght (NoBracket _) (NoBracket _) = True
cmpBracketLenght (NoBracket _) _ = False
cmpBracketLenght _ (NoBracket _) = False
cmpBracketLenght (Brackets _ b1) (Brackets _ b2) = cmpBracketLenght b1 b2

--typeToArrT (ArrayT t b) = tTATh t ((arrayFindDepth t) +1)

--tTATh :: Type -> Int -> LLVM.Size 
--tTATh t1 i = LLVM.SSize (((\(LLVM.SSize x) -> takeWhile (not.isDigit) x) (typeToArrT t1)) ++(show $ i - ( sum (zipWith (*) ( reverse ( map digitToInt ((\(LLVM.SSize x) -> filter isDigit x) (typeToArrT t1)))) [1,10..])))) 

-- Finds the array type for the input Bracket and type.
bracketToArrT :: Bracket -> Type -> LLVM.Size
bracketToArrT b t = case t of
    Int  -> LLVM.SSize ("%arrInt" ++ show (arrayFindDepth b))
    Doub -> LLVM.SSize ("%arrDoub" ++ show (arrayFindDepth b))
    Bool -> LLVM.SSize ("%arrBool" ++ show (arrayFindDepth b))
    (ArrayT t' b') -> bracketToArrT b t'

-- Finds the depth of the input Bracket
arrayFindDepth :: Bracket -> Int
arrayFindDepth (Brackets e b) = (arrayFindDepth b) + 1 
arrayFindDepth (NoBracket e) = 0


argTy :: Type -> LLVM.Size
argTy (ArrayT t _) = LLVM.SSize ( typeToArrT t ++ show 0)
argTy t = typeToItype t
        
-- Helps declar in the function "compileStm" to decide if variable is Initials or not 
declHelper :: Item -> Type -> CodeGen ()
declHelper (NoInit id) t = do
    extendContext id t
    r <- getVarReg id
    emit $ LLVM.Ass r (LLVM.Alloca (typeToItype t))
    case t of    
        Doub -> emit $ LLVM.Store (typeToItype t) (LLVM.VDoub 0.0) (typeToItype t) r
        _    -> emit $ LLVM.Store (typeToItype t) (LLVM.VInt 0) (typeToItype t) r
declHelper (Init id expr) t = do
    case expr of
        (ETyped (EArr t1@(ArrayT t e)) t2) -> do
            e' <- (compileExp expr)
            extendContextvVal id t1 e'
        (ETyped (EApp id1 e1s) (ArrayT t2 _)) -> do
            e' <- (compileExp expr)
            extendContextvVal id t e'    
        _       -> do
            e' <- (compileExp expr)
            extendContext id t
            r <- getVarReg id
            emit $ LLVM.Ass r (LLVM.Alloca (typeToItype t))
            emit $ LLVM.Store (typeToItype t) e' (typeToItype t) r

    
    
    

   
