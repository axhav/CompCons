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
  }

type Contexts = [VarContext]

-- | Variables in scope (symbol table)
data VarContext = VarContext
  { vars :: Map Id (LLVM.Val,Type)
  , next :: LLVM.Val
  }

type Sig = Map Id Def
  
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
            let c@VarContext{next=n} = last cont
            modify $ updateContexts $ (almostemptyContext n :)

exitBlock :: CodeGen ()
exitBlock = do  
    modify $ updateContexts $ tail

extendContext :: Id -> Type -> CodeGen ()
extendContext x t = modify $ updateContexts $ \ (b : bs) ->
  b { vars = Map.insert x (next b,t) (vars b)
    , next = next b + 1  -- TODO double
    } : bs
    

extendEnvSig :: Def -> CodeGen () 
extendEnvSig def@(DFun _t i _args _ss) = modify $ updateEnvSig i def
    
updateEnvSig :: Id -> Def -> Env -> Env 
updateEnvSig i def env = env {envSig = Map.insert i def (envSig env)}

lookupVar :: Id -> CodeGen (LLVM.Val,Type)
lookupVar x = do
  bs <- gets contexts
  case catMaybes $ map (Map.lookup x . vars) bs of
    []      -> error $ "unbound var " ++ printTree x
    (a : _) -> return a
    
lookDef :: Id -> CodeGen Def
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
  }

-- TODO (TÅDÅ) maybe remove

almoastEmptyEnv :: String -> Env
almoastEmptyEnv s = Env {className = s
  , envSig     = Map.empty
  , contexts   = []
  , code       = []
  , label      = 0
  }
  
updateContexts :: (Contexts -> Contexts) -> Env -> Env
updateContexts f env = env { contexts = f (contexts env) }

updateCode :: ([LLVM.Instruction] -> [LLVM.Instruction]) -> Env -> Env
updateCode f env = env { code = f (code env) }

updateLabel :: (LLVM.Label -> LLVM.Label) -> Env -> Env
updateLabel f env = env { label = f ( label env)}

-- * Contexts

emptyContext :: VarContext
emptyContext = VarContext
  { vars = Map.empty
  , next = 0
  }

-- TODO (TÅDÅ) maybe remove

almostemptyContext :: Int -> VarContext
almostemptyContext i = VarContext
  { vars = Map.empty
  , next = i
  }


-- Code Generator

codeGen :: FilePath -> Program -> String
codeGen filename prg = header ++ unlines (map LLVM.showInstruction lcode)
    where
        lcode = reverse $ code $ compileProgram prg `execState` emptyEnv
        header = unlines ["",""]

compileProgram :: Program -> CodeGen ()
compileProgram (TopDef defs) = do
    mapM_ extendEnvSig defs
    mapM_ compileDef defs

compileDef :: TopDef -> CodeGen ()
compileDef (FnDef t id args b) = do
    let t' = typeToItype t
    raw $ "define " ++ t' ++ " @"++ show id ++ "(" ++ showA args ++ ")"
    newBlock 
    compileBlock b
    exitBlock

showA :: Arg -> String
showA t id = typetoItype t ++ " %" ++ show id

typeToItype :: Type -> String
typeToItype Int  = "i32"
typeToItype Doub = "double"
typeToItype Bool = "i1"
typeToItype Void = "void"


compileBlock :: Block -> CodeGen ()
compileBlock (Block ss) = do
    emit $ raw "{"    
    mapM_ compileStm ss
    emit $ raw "}"

getNextReg :: Ident -> Type -> CodeGen (LLVM.Val)
getNextReg id t = do 
    extendContext id t
    (r,_) <- lookUpVar id
    return r

compileStm :: Stmt -> CodeGen ()
compileStm s = do
    blank
    comment $ removeNL $ printTree s
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
            r1 <- getNextReg id t
            compileExp r1 expr 

        (Incr id) -> do
            (VVal r1) <- getNextReg
            (_,t) <- lookUpVar id
            emit $ LLVM.Raw $ "%" ++ r1 ++ " = " ++ LLVM.showInstruction $ LLVM.Load $ (typeToItype t) (showReg id)
            (VVal r2) <- getNextReg
            emit $ LLVM.Raw $ "%" ++ r2 ++ " = " ++ LLVM.showInstruction $ LLVM.Add $ (typeToItype t) r1 (VInt 1) 
            emit $ LLVM.Store $ (typeToItype t) r2 (typeToItype t) (showReg id)
        (Decr id) -> do

        (Ret exp) -> do
        
        (VRet) -> do

        (Cond expr stm) -> do

        (CondElse expr stm1 stm2) -> do

        (While expr stm) -> do
            l1 <- getNextLabel
            l2 <- getNextLabel
            l3 <- getNextLabel
            r  <- getNextReg
            emit $ LLVM.Raw $ "L" ++ show l1 ++ ":" -- Label at top
            compileExp r expr
            emit $ LLVM.CondB (VVal r) l2 l3  
            emit $ LLVM.Raw $ "L" ++ show l2 ++ ":" -- Label after condition 
            compileStm stm
            emit $ LLVM.Goto l1
            emit $ LLVM.Raw $ "L" ++ show l3 ++ ":" -- Label out of while     
        
        (SExp expr) -> do
        
 
declHelper :: Item -> Type -> CodeGen ()
declHelper (NoInit id) t = emit $ LLVM.Raw $ (showReg id) ++ " = " ++ LLVM.showInstruction $ LLVM.Alloca $ typeToItype t
declHelper (Init id expr) t = do 
    emit $ LLVM.Raw $ (showReg id) ++ " = " ++ LLVM.showInstruction $ LLVM.Alloca $ typeToItype t
    emit $ LLVM.Store $ (typeToItype t) (compileExp expr) (typeToItype t) (showReg id)
    
showReg :: Ident -> String
showReg i = "%" ++ show i

compileExp :: LLVM.Val -> Exp -> CodeGen ()
compileExp (VVal r) (ETyped (ELitTrue) t) = emit $ LLVM.Raw r ++ " = i1 1"
compileExp (VVal r) (ETyped (ELitFalse) t) = emit $ LLVM.Raw r ++ "i1 0"
compileExp (VVal r) (ETyped (ELitInt i) t) = emit $ LLVM.Raw r ++ "i32 " ++ show i
compileExp (VVal r) (ETyped (ELitDoub d) t) = emit $ LLVM.Raw r ++ "double " show d
compileExp (VVal r) (ETyped (EVar id) t) = do
    (a,_) <- lookupVar id
    e

            




 





