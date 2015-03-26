module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
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

-- checks the definition of a function prepares its arguments into a new block of context and also 
-- defines a return type for that context
checkDef :: Env -> TopDef -> Err ()
checkDef env (FnDef t f args bl) = do
    let env' = foldl (\ env (Arg t x) -> extendCont env x t) (newBlock env) (take 1 args)--extendCont (newBlock env) (head args)
    env'' <- loop env' (drop 1 args) --foldl (\ env (Arg t x) -> checkDupe env x >> extendCont env x t) (newBlock env) args
    let env''' = extendCont env'' (Ident "return") t
    case checkBlock env''' bl of
        Ok a -> return ()
        Bad m -> fail m
    
    where loop e [] = return e
          loop e ((Arg t x):ars) =  do
                        checkDupe e x 
                        let env' = extendCont e x t
                        loop env' ars 
                        
checkBlock :: Env -> Block -> Err Env
checkBlock env (Block stms) = foldM checkStm env stms


checkStm :: Env -> Stmt -> Err Env
checkStm env s = case s of 
    Empty -> return env
    (BStmt b) -> do
        checkBlock (newBlock env) b
        return env
    (Decl t items) -> do
        loop t items env
        where 
            loop t [(NoInit x)] env = do
                checkDupe env x
                return $ extendCont env x t
            loop t [(Init x expr)] env = do
                checkDupe env x
                let env' = extendCont env x t
                inferExp env' expr
                return $ env'    
            loop t ((NoInit x):xs) env = do
                checkDupe env x 
                loop t xs (extendCont env x t)
            loop t ((Init x expr ):xs) env = do
                checkDupe env x 
                let env' = extendCont env x t
                inferExp env' expr
                loop t xs (extendCont env x t)
    (Ass id expr) ->  do
        t  <- lookVar env id
        t' <- inferExp env expr
        unless (t == t') $ fail $
            "expected numeric type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return env 
    (Incr id) -> do
        t <- lookVar env id
        unless (t `elem` [Int, Doub]) $ fail $
            "expected numberic type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return env
    (Decr id) -> do
        t <- lookVar env id
        unless (t `elem` [Int, Doub]) $ fail $
            "expected numberic type, but found " ++ printTree t ++
            " when checking " ++ printTree id
        return env
    (Ret expr) -> do
        rtype <- lookVar env (Ident "return")
        checkExp env expr rtype
        return env
    (VRet) -> return env 
    (Cond expr stm) -> do
        checkExp env expr Bool
        env' <- checkStm (newBlock env) stm
        return (exitBlock env')
    (CondElse expr stm1 stm2) -> do
        checkExp env expr Bool
        env' <- checkStm (newBlock env) stm1
        let env'' = exitBlock env'
        env''' <- checkStm (newBlock env) stm2
        return (exitBlock env''') 
    (While expr stm) -> do
        checkExp env expr Bool
        env' <- checkStm (newBlock env) stm 
        return (exitBlock env') 
    (SExp expr) -> env <$ inferExp env expr
    
    
checkExp :: Env -> Expr -> Type -> Err ()
checkExp env e t = do
    t' <- inferExp env e
    unless (t == t') $ fail $
        "expected type " ++ printTree t ++
        " but found type " ++ printTree t' ++
        " when checking expression " ++ printTree e


inferExp :: Env -> Expr -> Err Type
inferExp env e = case e of
    (EVar id) -> lookVar env id
    (ELitInt i) -> return Int 
    (ELitDoub d) -> return Doub
    (ELitTrue) -> return Bool
    (ELitFalse) -> return Bool
    (EApp id exprs)  -> case id of
        Ident "printString" -> 
            case exprs of
                [(EString s)] -> return Void 
                _           -> fail "expected string literal in function printString"
        _-> do
                FunType t ts <- lookDef env id
                unless (length exprs == length ts) $ fail $
                    "incorrect number of arguments to function " ++ printTree id
                t <$ zipWithM_ (checkExp env) exprs ts
    (EString str)   -> return Void
    (Neg expr)      -> inferExp env expr 
    (Not expr)      -> inferExp env expr
    (EMul e1 op e2) -> binaryNum env e1 e2 [Int,Doub]
    (EAdd e1 op e2) -> binaryNum env e1 e2 [Int,Doub]
    (ERel e1 op e2) -> binaryRel env e1 e2 op [Int,Doub,Bool] --TODO RM OP: Städa 
    (EAnd e1 e2)    -> binaryNum env e1 e2 [Bool]
    (EOr e1 e2)     -> binaryNum env e1 e2 [Bool]
    
    
-- Used to compare the type of numerical expression and makes it return the type of the inputted ones
binaryNum :: Env -> Expr -> Expr -> [Type] -> Err Type
binaryNum env e1 e2 ts = do
    t1 <- inferExp env e1
    t2 <- inferExp env e2
    unless (t1 == t2) $ fail $
            "expected type " ++ printTree t1 ++ " but found type " ++ printTree t2
        
    unless (t1 `elem` ts)  $ fail $
            "expected numeric type, but found " ++ printTree t1 ++
            " when checking " ++ printTree e1 ++ " and "++ printTree e2
    return t1 

binaryRel :: Env -> Expr -> Expr -> RelOp -> [Type] -> Err Type
binaryRel env e1 e2 op ts = do
    t1 <- inferExp env e1
    t2 <- inferExp env e2
    unless (t1 == t2) $ fail $
            "expected type " ++ printTree t1 ++ " but found type " ++ printTree t2
    unless (t1 `elem` ts)  $ fail $
            "expected numeric type, but found " ++ printTree t1 ++
            " when checking " ++ printTree e1 ++ " and "++ printTree e2
    return Bool
    

-- checks the current context to make sure that a variable name is not currently allready in scope
checkDupe :: Env -> Ident -> Err ()
checkDupe env@Env{envCont = b:bs} i = case Map.lookup i b of
    Just a -> fail $ "Variable already in scope " ++ printTree i ++ "  " ++ show b
    Nothing -> return ()
    
-- Checks the environment if a variable exists
lookVar :: Env -> Ident -> Err Type
lookVar env x = case catMaybes $ map (Map.lookup x) (envCont env) of
    []      -> fail $ "unbound variable " ++ printTree x
    (t : _) -> return t
-- checks the function definitions if a function exists                            
lookDef :: Env -> Ident -> Err FunType
lookDef env f = case Map.lookup f (envSig env) of
    Nothing -> fail $ "undefined function " ++ printTree f
    Just t  -> return t
-- adds a new function definition to the environment if it does not allready exist
extendSig :: Env -> TopDef -> Err Env
extendSig env@Env{ envSig = sig } (FnDef t f args _ss) = do
    if Map.member f sig then fail $ "duplicate definition of function " ++ printTree f else return env { envSig = Map.insert f ft (envSig env) }
    where ft = FunType t $ map (\ (Arg t _x) -> t) args

-- adds a new id to the current context of the environment
extendCont :: Env -> Ident -> Type -> Env
extendCont env@Env{ envCont = b : bs } x t = env { envCont = Map.insert x t b : bs }
-- adds a new context to the environment 
newBlock :: Env -> Env
newBlock env = env { envCont = Map.empty : envCont env }
-- removes the current context from the environment 
exitBlock :: Env -> Env
exitBlock env@Env { envCont = b : bs } = env { envCont = bs }
-- creates an empty envoronment 
emptyEnv :: Env
emptyEnv = Env { envSig = Map.empty, envCont = [Map.empty] }

    
-- typechecks the program 
typecheck :: Program -> Err ()
typecheck (Program defs) = do
    let env0 = Env{ envSig = Map.fromList [(Ident "printInt", FunType Void [Int]),
            (Ident "printDouble", FunType Void [Doub]),(Ident "readInt", FunType Int [])
            ,(Ident "readDouble", FunType Doub [])]--,(Ident "printString",FunType Void [EString ""])]
        , envCont = []
        }
    env <- foldM extendSig env0 defs    
    mapM_ (checkDef env) defs
