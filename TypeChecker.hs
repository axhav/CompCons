{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor
import Data.Maybe

-- Data structure to define the environment 
data Env = Env {
    envSig :: Sig
    ,envCont :: Cont
    }
-- Defines a map between function ids and function types
type Sig = Map.Map Ident FunType
type Cont = [EBlock]
-- defines a map between ids and their types
type EBlock = Map.Map Ident Type 
-- Defines a function type
data FunType = FunType Type [Type]



newtype EnvM a = EnvM { unEnv :: StateT Env Err a }
    deriving (Functor,Monad)
    
    
    
-- Checks the definition of a function prepares its arguments 
-- into a new block of context and also defines a return
-- type for that context
checkDef :: TopDef -> EnvM TopDef
checkDef (FnDef t f args bl) = do
    newBlock
    extendCont (Ident "return") t
    case args of
        [] -> do
            ret <- (checkBlock bl) 
            return $ FnDef t f args ret  
        ((Arg t' x):xs)-> do
            extendCont x t'   --extendCont (newBlock env) (head args)
            loop (drop 1 args) --foldl (\ env (Arg t x) -> checkDupe env x >> extendCont env x t) (newBlock env) args
            ret <- (checkBlock bl) 
            
            return $ FnDef t f args ret
    
    where loop [] = return ()
          loop ((Arg t x):ars) =  do
                        checkDupe x
                        extendCont x t
                        loop ars 
          
-- Checks input block              
checkBlock :: Block -> EnvM Block
checkBlock (Block stms) =  do
    ret <-(mapM checkStm stms)
    return $ Block ret 

-- Checks input statment
checkStm :: Stmt -> EnvM Stmt
checkStm s = case s of 
    Empty -> return Empty
    (BStmt b) -> do
        newBlock 
        b' <- checkBlock b
        exitBlock
        return (BStmt b')  
    (Decl t items) -> do
        loopHelper t items
        return (Decl t items)
    (Ass id expr) ->  do
        t <- lookVar id
        ret@(ETyped e t') <- inferExp expr           
        unless (t == t') $ fail $
            "expected numeric type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return (Ass id ret)
    (Incr id) -> do
        t <- lookVar id
        unless (t `elem` [Int, Doub]) $ fail $
            "expected numberic type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return (Incr id)
    (Decr id) -> do
        t <- lookVar id
        unless (t `elem` [Int, Doub]) $ fail $
            "expected numberic type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return (Decr id)
    (Ret expr) -> do
        rtype <- lookVar (Ident "return")
        expr' <- checkExp expr rtype
        return (Ret expr')
    (VRet) -> return (VRet)
    (Cond expr stm) -> do
        expr' <- checkExp expr Bool
        newBlock
        stm'<- checkStm stm
        exitBlock
        return (Cond expr' stm')
    (CondElse expr stm1 stm2) -> do
        expr' <- checkExp expr Bool
        newBlock
        stm1' <- checkStm stm1
        newBlock 
        exitBlock
        stm2' <- checkStm stm2
        exitBlock
        return (CondElse expr' stm1' stm2')
    (While expr stm) -> do
        expr' <- checkExp expr Bool
        newBlock
        stm' <- checkStm stm 
        exitBlock 
        return  (While expr' stm') 
    (SExp expr) -> do 
        e <- (inferExp expr)
        return (SExp e )
    
-- Help function for statment Decl
loopHelper:: Type -> [Item] -> EnvM ()
loopHelper t [(NoInit x)] = do
    checkDupe x
    extendCont x t
loopHelper t [(Init x expr)] = do
    checkDupe x
    checkExp expr t
    extendCont x t    
loopHelper t ((NoInit x):xs) = do
    checkDupe x 
    extendCont x t
    loopHelper t xs 
loopHelper t ((Init x expr ):xs) =  do
    checkDupe x 
    checkExp expr t
    extendCont x t
    loopHelper t xs
    
-- Checks experssion
checkExp :: Expr -> Type -> EnvM Expr
checkExp e t = do
    ret@(ETyped e t') <- inferExp e
    unless (t == t') $ fail $
        "expected type " ++ printTree t ++
        " but found type " ++ printTree t' ++
        " when checking expression " ++ printTree e
    return ret


inferExp :: Expr -> EnvM Expr
inferExp e = case e of
    (EVar id) -> do
        t <- (lookVar id)
        return (ETyped (EVar id) t)
    (ELitInt i) -> return (ETyped (ELitInt i) Int) 
    (ELitDoub d) -> return (ETyped (ELitDoub d) Doub)
    (ELitTrue) -> return (ETyped (ELitTrue) Bool)
    (ELitFalse) -> return (ETyped (ELitFalse) Bool)
    (EApp id exprs)  -> case id of
        Ident "printString" -> 
            case exprs of
                [(EString s)] -> return (ETyped (EApp id exprs) Void)
                _           -> fail "expected string literal in function printString"
        _-> do
                FunType t ts <- lookDef id
                unless (length exprs == length ts) $ fail $
                    "incorrect number of arguments to function " ++ printTree id
                ts' <- (zipWithM checkExp exprs ts)
                return (ETyped (EApp id ts') t) 
    (EString str)   -> return (EString str) --  TODO look at this
    (Neg expr)      ->do
        ret@(ETyped _ t) <- (inferExp expr)
        return (ETyped (Neg ret) t) 
    (Not expr)      -> do
        ret@(ETyped _ t) <- inferExp expr
        return (ETyped (Not ret) t)
    (EMul e1 op e2) -> do
        (e1', e2',t) <- binaryNum e1 e2 [Int,Doub]
        return (ETyped  (EMul e1' op e2') t)
    (EAdd e1 op e2) -> do
        (e1',e2',t) <- binaryNum e1 e2 [Int,Doub]
        return (ETyped  (EAdd e1' op e2') t)
    (ERel e1 op e2) -> do
        (e1',e2',t) <- binaryRel e1 e2 op [Int,Doub,Bool] --TODO RM OP: Städa 
        --fail $ show e1'
        return (ETyped  (ERel e1' op e2') t)
    (EAnd e1 e2)    -> do
        (e1',e2',t) <- binaryNum e1 e2 [Bool]
        return (ETyped  (EAnd e1' e2') t)
    (EOr e1 e2)     -> do
        (e1',e2',t) <- binaryNum e1 e2 [Bool]
        return (ETyped  (EOr e1' e2') t)
    
    
-- Used to compare the type of numerical expression and makes it 
-- return the type of the inputted ones
binaryNum :: Expr -> Expr -> [Type] -> EnvM (Expr, Expr ,Type)
binaryNum e1 e2 ts = do
    ETyped e1' t1 <- inferExp e1
    ETyped e2' t2 <- inferExp e2
    unless (t1 == t2) $ fail $
            "expected type " ++ printTree t1 ++ " but found type " ++ printTree t2
        
    unless (t1 `elem` ts)  $ fail $
            "expected numeric type, but found " ++ printTree t1 ++
            " when checking " ++ printTree e1 ++ " and "++ printTree e2
    return  (e1',e2', t1) 

binaryRel :: Expr -> Expr -> RelOp -> [Type] -> EnvM (Expr, Expr ,Type)
binaryRel e1 e2 op ts = do
    exp1@(ETyped e1' t1) <- inferExp e1
    exp2@(ETyped e2' t2) <- inferExp e2
    --fail $ printTree exp1 ++ "    " ++ printTree exp2
    unless (t1 == t2) $ fail $
            "expected type " ++ printTree t1 ++ " but found type " ++ printTree t2
    unless (t1 `elem` ts)  $ fail $
            "expected numeric type, but found " ++ printTree t1 ++
            " when checking " ++ printTree exp1 ++ " and "++ printTree exp2
    return (e1',e2', Bool)
    

-- Checks the current context to make sure that a 
-- variable name is not currently allready in scope
checkDupe :: Ident -> EnvM ()
checkDupe i = EnvM $ do
    env <- get
    let env'@Env{envCont = b:bs} = env
    case Map.lookup i b of
        Just a -> fail $ "Variable" ++ printTree i ++ " already in scope " ++ show b
        Nothing -> return ()
    
-- Checks the environment if a variable exists
lookVar :: Ident -> EnvM Type
lookVar x = EnvM $do
    env <- gets envCont 
    case catMaybes $ map (Map.lookup x) (env) of
        []      -> fail $ "unbound variable " ++ printTree x
        (t : _) -> return t
-- Checks the function definitions if a function exists                            
lookDef ::Ident -> EnvM FunType
lookDef f =EnvM $ do
    env <- gets envSig
    case Map.lookup f ( env) of
        Nothing -> fail $ "undefined function " ++ printTree f
        Just t  -> return t
-- Adds a new function definition to the environment 
-- if it does not allready exist
extendSig :: Env ->  TopDef -> Err Env
extendSig env@Env{ envSig = sig } (FnDef t f args _ss) = do
    if Map.member f sig then 
		fail $ "duplicate definition of function " ++ printTree f 
    else 
        return env { envSig = Map.insert f ft (envSig env) }
    where ft = FunType t $ map (\ (Arg t _x) -> t) args

-- Adds a new id to the current context of the environment
--extendCont :: Env -> Ident -> Type -> Env
--extendCont env@Env{ envCont = b : bs } x t = env { envCont = Map.insert x t b : bs }

extendCont :: Ident -> Type -> EnvM ()
extendCont x t = EnvM $ do
    env@Env{ envCont = b : bs } <- get
    put env {envCont = Map.insert x t b : bs }

-- Adds a new context to the environment 
newBlock :: EnvM ()
newBlock = EnvM $ do
    env <- get 
    put $ env { envCont = Map.empty : envCont env }

-- Removes the current context from the environment 
exitBlock :: EnvM ()
exitBlock = EnvM $ do
    env@Env { envCont = b : bs }<- get
    put $ env { envCont = bs }
-- Creates an empty envoronment 
emptyEnv :: Env
emptyEnv = Env { envSig = Map.empty, envCont = [Map.empty] }

    
-- Typechecks the program 
typecheck :: Program -> Err Program
typecheck (Program defs) = do
    let env0 = Env{ envSig = Map.fromList [(Ident "printInt", FunType Void [Int]),
            (Ident "printDouble", FunType Void [Doub]),(Ident "readInt", FunType Int [])
            ,(Ident "readDouble", FunType Doub [])]--,(Ident "printString",FunType Void [EString ""])]
        , envCont = []
        }
    env <- foldM extendSig env0 defs 

    --(a,env') <- runStateT checkDef defs env 
    --ret <- checkDef env defs
    ret' <- (help env defs)
    return $ Program ret'

help :: Env -> [TopDef] -> Err [TopDef]
help env [] = return []
help env (def:defs) = case runStateT (unEnv (checkDef def)) env of
        Ok a -> do
            b <- help (snd a) defs
            return $ (fst a): b
        Bad m ->  fail m
    
    
