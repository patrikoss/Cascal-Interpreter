{-# LANGUAGE DeriveDataTypeable #-}
module Interpreter where

import AbsCascal
import PrintCascal
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict
import Control.Monad(foldM, forM_)
import Control.Monad.Trans(liftIO)
import Data.Maybe(fromJust)
import Text.Read(readMaybe)
import Control.Exception
import Data.Typeable.Internal as DTI

data IVal = IInt Integer
            | IBool Bool
            | IString String
            | IFun Env [Ident] [Instr] Exp
            deriving(Eq)
            
instance Show IVal where
    show (IInt i) = show i
    show (IBool b) = show b
    show (IString s) = s
    show (IFun _ _ _ _) = "IFun"

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc IVal
type Context = (Env, Store, Loc)

data RuntimeError = UninitializedVariableException Ident |
                    UnconvertibleToIntException String   |
                    DivisionByZeroException Exp
                    deriving(Show, DTI.Typeable)
                    
handleRuntimeError :: RuntimeError -> IO ()
handleRuntimeError err = case err of
    UninitializedVariableException id ->
        error $ "Uninitialized Variable Exception\n" ++
            "Variable '" ++ (printTree id) ++ "' has not been initialized."
    UnconvertibleToIntException s ->
        error $ "Unconvertible to int exception\n" ++
            "Cannot convert '" ++ s ++ "' to int."
    DivisionByZeroException exp ->
        error $ "Division by zero exception\n" ++
            "'" ++ (printTree exp) ++ "' is not a valid expression\n" ++
            "Denominator evaluates to 0"
                    
instance Exception RuntimeError ;
                    

-------------------------------- Helper functions -----------------------------
getIdentLoc :: Ident -> StateT Context IO Loc
getIdentLoc id = do
    (env, _, _) <- get
    return $ fromJust $ M.lookup id env 
    
getIdentIVal :: Ident -> StateT Context IO IVal
getIdentIVal id = do
    (_, store, _) <- get
    idLoc <- getIdentLoc id
    case M.lookup idLoc store of
        Just ival -> return ival
        Nothing -> liftIO $ throwIO $ UninitializedVariableException id


------------------------------------ Expressions -----------------------------
evalExp :: Exp -> StateT Context IO IVal

----------------------------- Boolean expressions -----------------------------
evalExp (EBool (Boolean "false")) = return $ IBool False
evalExp (EBool (Boolean "true")) = return $ IBool True

evalExp (EOr e1 e2) = do
    IBool val1 <- evalExp e1
    IBool val2 <- evalExp e2
    return $ IBool $ val1 || val2
    
evalExp (EAnd e1 e2) = evalExp $ ENot $ EOr (ENot e1) (ENot e2)

evalExp (EEq e1 e2) = do
    ival1 <- evalExp e1
    ival2 <- evalExp e2
    return $ IBool $ ival1 == ival2  
    
evalExp (ENeq e1 e2) = evalExp $ ENot $ EEq e1 e2

evalExp (ELt e1 e2) = do
    ival1 <- evalExp e1
    ival2 <- evalExp e2
    case (ival1, ival2) of
        (IInt i1, IInt i2) -> return $ IBool $ i1 < i2
        (IString s1, IString s2) -> return $ IBool $ s1 < s2

evalExp(EGt e1 e2) = evalExp $ EAnd (ENot $ ELt e1 e2) (ENeq e1 e2)

evalExp(ELe e1 e2) = evalExp $ EOr (EEq e1 e2) (ELt e1 e2)

evalExp(EGe e1 e2) = evalExp $ EOr (EEq e1 e2) (EGt e1 e2)

evalExp(ENot e) = do
    IBool b <- evalExp e
    return $ IBool $ not b
    
----------------------------- Arithmetic expressions --------------------------

evalExp (EInt i) = return $ IInt i;

evalExp(EAdd e1 e2) = do
    IInt ival1 <- evalExp e1
    IInt ival2 <- evalExp e2
    return $ IInt $ ival1 + ival2

evalExp(EMinus e1 e2) = evalExp $ EAdd e1 (ENeg e2)

evalExp(EMul e1 e2) = do
    IInt ival1 <- evalExp e1
    IInt ival2 <- evalExp e2
    return $ IInt $ ival1 * ival2

evalExp(EDiv e1 e2) = do
    IInt ival1 <- evalExp e1
    IInt ival2 <- evalExp e2
    if ival2 == 0 then liftIO $ throwIO $ DivisionByZeroException (EDiv e1 e2)
    else return $ IInt $ ival1 `div` ival2

evalExp(ENeg e) = do
    IInt ival <- evalExp e
    return $ IInt $ -ival

-- pre/post incrementation/decrementation
evalExp(EPreInc id) = do
    evalStmt (SAssign (LhsIdent id) AssignAdd (EInt 1))
    evalExp (EIdent id)
    
evalExp(EPreDec id) = do
    evalStmt (SAssign (LhsIdent id) AssignSub (EInt 1))
    evalExp (EIdent id)

evalExp(EPostInc id) = do
    ival <- evalExp (EIdent id)
    evalStmt (SAssign (LhsIdent id) AssignAdd (EInt 1))
    return ival

evalExp(EPostDec id) = do
    ival <- evalExp (EIdent id)
    evalStmt (SAssign (LhsIdent id) AssignSub (EInt 1))
    return ival



-- String expression
evalExp (EString s) = return $ IString s

evalExp (EStrToInt e) = do
    IString s <- evalExp e
    case readMaybe s :: Maybe Integer of
        Just t -> return $ IInt t
        Nothing -> liftIO $ throwIO $ UnconvertibleToIntException s

evalExp (EIntToStr e) = do
    IInt i <- evalExp e
    return $ IString $ show(i)
        
-- Ident expression
evalExp (EIdent id) = getIdentIVal id
    
---------------------------- Function expressions -----------------------------
evalExp (ECall id args) = do
    (envStart, _, _) <- get
    IFun envDecl paramIds instrs retExp <- getIdentIVal id
    evArgs <- mapM evalExp args

    -- put env from function declaration
    (env, store, freeLoc) <- get
    put(envDecl, store, freeLoc)
    
    forM_ (zip paramIds evArgs) (\(pid, pexp) -> do 
        (env, store, freeLoc) <- get
        put(M.insert pid freeLoc env, M.insert freeLoc pexp store, freeLoc+1)
        )
    
    evalMultInstr instrs
    callExp <- evalExp retExp
    
    (_, store'', freeLoc'') <- get
    put(envStart, store'', freeLoc'')
    return callExp
    
    
evalExp (ELamFun t param body) = do
    (env,store,freeLoc) <- get
    evalDecl(DeclFunc t (Ident "_lamFun") param body)
    lamIVal <- getIdentIVal (Ident "_lamFun")
    put(env,store,freeLoc)
    return lamIVal
    
evalExp (ELamApp (ELamFun t param body) args) = do
    (env,_,_) <- get
    evalDecl(DeclFunc t (Ident "_lamFun") param body)
    iValCall <- evalExp (ECall (Ident "_lamFun") args)
    (_,store',freeLoc') <- get
    put(env,store',freeLoc')
    return iValCall
    
--------------------------------- Declarations -------------------------------- 

evalDecl :: Decl -> StateT Context IO ()

evalDecl (DeclAny t id) = do
    (env,store,freeLoc) <- get  
    put(M.insert id freeLoc env, store, freeLoc+1)
    
evalDecl (DeclInit t id exp) = do
    evalDecl (DeclAny t id)
    evalStmt (SAssign (LhsIdent id) Assign exp)

evalDecl (DeclFunc t id param (FuncBody instrs exp)) = do
    (env, store, freeLoc) <- get
    let f = (IFun (M.insert id freeLoc env) 
            (map (\(ParamDecl t id) -> id) param) instrs exp)
    put(M.insert id freeLoc env, 
        M.insert freeLoc f store,
        freeLoc+1)
---------------------------------- Statements ---------------------------------

evalStmt :: Stmt -> StateT Context IO ()

evalStmt SPass = return ()

evalStmt (SAssign lhs@(LhsIdent id) assignOp exp) = do
    let ret = case assignOp of
            Assign    -> evalExp exp
            AssignAdd -> evalExp (EAdd (EIdent id) exp)
            AssignSub -> evalExp (EMinus (EIdent id) exp)
            AssignMul -> evalExp (EMul (EIdent id) exp)
            AssignDiv -> evalExp (EDiv (EIdent id) exp)
            AssignAnd -> evalExp (EAnd (EIdent id) exp)
            AssignOr  -> evalExp (EOr (EIdent id) exp)
    retVal <- ret
    idLoc <- getIdentLoc id
    (env,store,freeLoc) <- get    
    put(env, M.insert idLoc retVal store, freeLoc)

evalStmt (SIf e instr) = do
    (env, store, freeLoc) <- get
    IBool b <- evalExp e
    if b then evalInstr (InstrBlock [instr]) else return ()

evalStmt (SIfElse e instr1 instr2) = do
    (env, store, freeLoc) <- get
    IBool b <- evalExp e
    if b then evalInstr (InstrBlock [instr1]) 
    else evalInstr (InstrBlock [instr2])
    
evalStmt swhile@(SWhile e instr) = do
    (env, store, freeLoc) <- get
    IBool b <- evalExp e
    if (b) then do
        evalInstr (InstrBlock [instr])
        evalStmt swhile
    else return()
    
evalStmt (SForTo lhs efrom eto instr) = do
    IInt itFrom <- evalExp efrom
    IInt itTo <- evalExp eto
    if itFrom <= itTo then do
        evalStmt (SAssign lhs Assign (EInt itFrom))
        evalInstr (InstrBlock [instr])
        evalStmt $ SForTo lhs (EInt (itFrom + 1)) (EInt itTo) instr
    else return ()
            
evalStmt (SForDownTo lhs efrom eto instr) = do
    IInt itFrom <- evalExp efrom
    IInt itTo <- evalExp eto
    if itFrom >= itTo then do
        evalStmt (SAssign lhs Assign (EInt itFrom))
        evalInstr (InstrBlock [instr])
        evalStmt $ SForDownTo lhs (EInt (itFrom - 1)) (EInt itTo) instr
    else return ()

evalStmt (SPrint exp) = do
    ival <- evalExp exp
    liftIO $ print ival
    
-------------------------------- Instructions ---------------------------------
evalMultInstr :: [Instr] -> StateT Context IO ()
evalMultInstr is =  foldM (\dummy instr -> evalInstr instr) () is

evalLocally :: [Instr] -> StateT Context IO ()
evalLocally is = do
    (env,_,_) <- get
    evalMultInstr is
    (_,store',freeLoc') <- get
    put(env, store', freeLoc')

evalInstr :: Instr -> StateT Context IO ()
evalInstr (InstrDecl d) = evalDecl d
evalInstr (InstrStmt stmt) = evalStmt stmt
evalInstr (InstrBlock is) = evalLocally is

------------------------------- Program ---------------------------------------
evalProg :: Program -> StateT Context IO ()
evalProg (Prog is) = evalMultInstr is
