{-# LANGUAGE DeriveDataTypeable #-}
module Validator where

import AbsCascal
import PrintCascal
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict
import Control.Exception
import Control.Monad.Trans(liftIO)
import Control.Monad(foldM, forM_)
import Data.List(nub)
import Text.Read(readMaybe)
import Data.Typeable.Internal as DTI

type Context = M.Map Ident VType

data VType
    = VInt | VBool | VString | VFun VType [VType]
  deriving (Eq, Show, Read)

data ValidatorError = DistinctExpressionTypeException [Exp] |
                      UnallowedTypeException [VType] [Exp] |
                      IncorrectArgumentNumberException Ident [Exp] Int [VType] Int |
                      ArgumentTypeMismatchException Ident [Exp] Exp VType VType [VType] |
                      NotAFunctionException Ident |
                      NotALambdaException Exp [Exp] |
                      UndeclaredIdentifierException Ident |
                      ParameterCollisionException Ident [Param] |
                      AssignmentTypeMismatchException Lhs VType AssignOp Exp VType |
                      IdentTypeException Ident VType VType Exp
                      deriving(Show, DTI.Typeable)

 
handleValidatorError :: ValidatorError -> IO ()
handleValidatorError err = case err of
    DistinctExpressionTypeException exps ->
        error $ "Distinct type expression exception\n" ++ 
            "Following expressions are of different types:\n[" ++
            (printTree exps) ++ "]"
    UnallowedTypeException vtypes exps ->
        error $ "Unallowed type exception\n" ++
            "Following expressions:\n[" ++ (printTree exps) ++
            "]\nmust be of one of the following type:\n"++
            (show vtypes)
    IncorrectArgumentNumberException id args argsLen paramTypes paramsLen ->
        error $ "Incorrect number of arguments in call to function '" ++ (printTree id) ++ "'\n" ++
            "Expected arguments: " ++ (show paramsLen) ++ ": " ++ (show paramTypes) ++ "\n" ++
            "Passed arguments: " ++ (show argsLen) ++ ": [" ++ (printTree args) ++ "]"
    ArgumentTypeMismatchException fname args arg argType paramType paramTypes ->
        error $ "Argument type mismatch exception in call to function '" ++
            (printTree fname) ++ "' with arguments [" ++ (printTree args) ++ "]\n" ++
            "The function expects arguments of types: " ++ (show paramTypes) ++ "\n" ++ 
            "Argument which caused conflict: " ++ (printTree arg) ++ "\n" ++
            "Type of argument which caused conflict: " ++ (show argType) ++ "\n" ++
            "Expected type: " ++ (show paramType) ++ "\n"            
    NotAFunctionException id ->
        error $ "Not a function exception\n" ++
            "Cannot call a function '" ++ (printTree id) ++ "'\n" ++
            "'" ++ (printTree id) ++ "' is not a function"
    NotALambdaException exp args ->
        error $ "Not a lambda exception\n" ++
            "Cannot apply '" ++ (printTree exp) ++ "' to arguments [" ++
            (printTree args) ++ "].\n'" ++ (printTree exp) ++ "' is not a" ++
            "lambda function"
    UndeclaredIdentifierException id ->
        error $ "Identifier '" ++ (printTree id) ++ "' has not been declared."
    ParameterCollisionException id params ->
        error $ "In function declaration '" ++ (printTree id) ++ "' " ++
            "parameters' names collide:\n" ++ "[" ++ (printTree params) ++ "]"
    AssignmentTypeMismatchException (LhsIdent id) idType assignOp exp expType -> do
        error $ "Assignment type mismatch in: \n" ++ 
            (printTree id) ++ " " ++ (printTree assignOp) ++ " " ++ 
                (printTree exp) ++ "\n" ++
            "Identifier '" ++ (printTree id) ++ "'" ++ " is of type: "
                 ++ (show idType) ++ "\n" ++
            "Expression '" ++ (printTree exp) ++ "' is of type: " ++ show(expType)
    IdentTypeException id idType expectedType exp ->
        error $ "Identifier '" ++ show(id) ++ 
            "' is not of the supposed type in expression" ++ (printTree exp) ++
            ". Expected type: " ++ show(expectedType) ++ 
            ". Identifier type: " ++ show(idType)
            
instance Exception ValidatorError ;

getIdentType :: Ident -> StateT Context IO VType
getIdentType id = do
    env <- get
    case M.lookup id env of
        Just t -> return t
        Nothing -> liftIO $ throwIO $ UndeclaredIdentifierException id


------------------------------ EXPRESSIONS ------------------------------------
----------------------- Expressions do not change the env ---------------------
assertExpSameType :: [Exp] -> StateT Context IO ()
assertExpSameType exps = do
    expTypes <- mapM getExpType exps
    if length(nub expTypes) < 2 then return ()
    else liftIO $ throwIO $ DistinctExpressionTypeException exps

assertExpAllowedType :: [VType] -> [Exp] -> StateT Context IO ()
assertExpAllowedType allowed exps = do
    expTypes <- mapM getExpType exps
    if all (\t -> t `elem` allowed) expTypes then return ()
    else liftIO $ throwIO $ UnallowedTypeException allowed exps
    
assertExpSameAndAllowedType :: [VType] -> [Exp] -> StateT Context IO ()
assertExpSameAndAllowedType allowed exps = do
    assertExpSameType exps
    assertExpAllowedType allowed exps
    
assertIdentType :: Ident -> VType -> Exp -> StateT Context IO ()
assertIdentType id vt exp = do
    idType <- getIdentType id
    if(idType == vt) then return ()
    else liftIO $ throwIO $ IdentTypeException id idType vt exp

getExpType :: Exp -> StateT Context IO VType
getExpType (EOr e1 e2) = do
    assertExpSameAndAllowedType [VBool] [e1, e2]
    return VBool
getExpType (EAnd e1 e2) = do
    assertExpSameAndAllowedType [VBool] [e1, e2]
    return VBool
getExpType (EEq e1 e2) = do
    assertExpSameAndAllowedType [VInt, VBool, VString] [e1, e2]
    return VBool
getExpType (ENeq e1 e2) = do
    assertExpSameAndAllowedType [VInt, VBool, VString] [e1, e2]
    return VBool
getExpType (ELt e1 e2) = do
    assertExpSameAndAllowedType [VInt,VString] [e1, e2]
    return VBool    
getExpType (EGt e1 e2) = do
    assertExpSameAndAllowedType [VInt,VString] [e1,e2]  
    return VBool      
getExpType (ELe e1 e2) = do
    assertExpSameAndAllowedType [VInt,VString] [e1, e2]
    return VBool
getExpType (EGe e1 e2) = do
    assertExpSameAndAllowedType [VInt, VString] [e1,e2]
    return VBool
getExpType (ENot e)    = do
    assertExpAllowedType [VBool] [e]
    return VBool
getExpType (EBool e)   = return VBool

getExpType (EAdd e1 e2) = do
    assertExpSameAndAllowedType [VInt] [e1,e2]
    return VInt
getExpType (EMinus e1 e2) = do
    assertExpSameAndAllowedType [VInt] [e1,e2]
    return VInt
getExpType (EMul e1 e2) = do
    assertExpSameAndAllowedType [VInt] [e1,e2]
    return VInt
getExpType (EDiv e1 e2) = do
    assertExpSameAndAllowedType [VInt] [e1,e2]
    return VInt
getExpType (ENeg e) = do
    assertExpAllowedType [VInt] [e]
    return VInt
getExpType (EInt e)   = return VInt

getExpType exp@(EPreInc id) = do
    assertIdentType id VInt exp
    return VInt

getExpType exp@(EPreDec id) = do
    assertIdentType id VInt exp
    return VInt

getExpType exp@(EPostInc id) = do
    assertIdentType id VInt exp
    return VInt

getExpType exp@(EPostDec id) = do
    assertIdentType id VInt exp
    return VInt

getExpType (EString e)   = return VString

getExpType (EStrToInt e) = do
    assertExpAllowedType [VString] [e]
    return VInt
    
getExpType (EIntToStr e) = do
    assertExpAllowedType [VInt] [e]
    return VString

getExpType (EIdent id) = do
    idType <- getIdentType id
    return idType
    
getExpType (ELamFun t param body) = do
    env <- get
    validateDecl (DeclFunc t (Ident "_lamFun") param body)
    lamType <- getIdentType (Ident "_lamFun")
    put(env)
    return lamType
    
getExpType (ELamApp (ELamFun t param body) args) = do
    env <- get
    validateDecl (DeclFunc t (Ident "_lamFun") param body)
    retType <- getExpType(ECall (Ident "_lamFun") args)
    put(env)
    return retType

getExpType (ELamApp exp args) = liftIO $ throwIO $ NotALambdaException exp args
    
getExpType fcall@(ECall id args) = do
    idType <- getIdentType id
    case idType of
        VFun retType paramType -> do
            -- check correct argument number
            if length(paramType) == length(args) then return ()
            else liftIO $ throwIO $ IncorrectArgumentNumberException id args (length args) paramType (length paramType)
            
            -- check if type of the arguments are correct
            forM_ (zip paramType args) (\(pT, arg) -> do
                    argT <- getExpType arg
                    if argT == pT then return ()
                    else liftIO $ throwIO $ ArgumentTypeMismatchException id args arg argT pT paramType
                )
                
            -- return the correct type
            return retType
        
        -- if it's not a function then we report and error 
        otherwise -> liftIO $ throwIO $ NotAFunctionException id


------------------------------ STATEMENTS -------------------------------------
validateStmt :: Stmt -> StateT Context IO ()

validateStmt SPass = return ()

validateStmt (SAssign lhs@(LhsIdent id) assignOp exp) = do
    idType <- getIdentType id
    expType <- getExpType exp
    if(expType == idType) then return ()
    else liftIO $ throwIO $ AssignmentTypeMismatchException lhs idType assignOp exp expType

validateStmt (SIf e instr) = do
    assertExpAllowedType [VBool] [e]
    validateLocally [instr]
  
validateStmt (SIfElse e instr1 instr2) = do
    assertExpAllowedType [VBool] [e]
    validateLocally [instr1]
    validateLocally [instr2]
    
validateStmt (SWhile e instr) = do
    assertExpAllowedType [VBool] [e]
    validateLocally [instr]
    
validateStmt (SForTo lhs efrom eto instr) = do
    validateStmt (SAssign lhs Assign efrom)
    efromType <- getExpType efrom
    etoType <- getExpType eto
    assertExpSameAndAllowedType [VInt] [efrom, eto]
    validateLocally [instr]
    
validateStmt (SForDownTo lhs efrom eto instr) =
    validateStmt (SForTo lhs efrom eto instr)

validateStmt (SPrint exp) = assertExpAllowedType [VBool, VInt, VString] [exp]

------------------------------ DECLARATIONS -----------------------------------


mapTypeToVType :: Type -> VType
mapTypeToVType t = 
    case t of
        TInt -> VInt
        TBool -> VBool
        TString -> VString
        TFun ret paramTypes -> VFun (mapTypeToVType ret)
            (map (\pt -> mapTypeToVType pt) paramTypes)

validateDecl :: Decl -> StateT Context IO ()
validateDecl (DeclAny t id) = do
    env <- get
    put(M.insert id (mapTypeToVType t) env)
    
validateDecl (DeclInit t id exp) = do
    validateDecl (DeclAny t id)
    validateStmt (SAssign (LhsIdent id) Assign exp)
    
validateDecl (DeclFunc t id param (FuncBody instrs exp)) = do
    envStart <- get
    let 
        retType = mapTypeToVType t
        vt = mapTypeToVType $ TFun t $ map (\(ParamDecl pt pid) -> pt) param
        idList = foldl (\acc (ParamDecl pt pid) -> pid:acc) [id] param

    -- make sure that params id and function id are all different
    case (length(idList) == length(nub(idList))) of
        False -> liftIO $ throwIO $ ParameterCollisionException id param 
        True -> return ()
    
    -- insert parameters into env
    forM_ param (\(ParamDecl pt pid) -> do 
            env <- get
            put(M.insert pid (mapTypeToVType pt) env)
        )
        
    -- insert func into env(in orderd to allow for recursion)
    env <- get
    put(M.insert id vt env)

    -- validate the instructions inside the function with current env is fine
    validateMultInstr instrs
    
    -- make sure the function return type matches the type of return expression
    assertExpAllowedType [retType] [exp]
    
    -- restore initial environment saving only identifier of the function 
    put(M.insert id vt envStart)

------------------------------ INSTRUCTIONS -----------------------------------
validateMultInstr :: [Instr] -> StateT Context IO ()
validateMultInstr is =  foldM (\dummy instr -> validateInstr instr) () is
    
validateLocally :: [Instr] -> StateT Context IO ()
validateLocally is = do
    env <- get
    validateMultInstr is
    put(env)

validateInstr :: Instr -> StateT Context IO ()

validateInstr (InstrDecl d) = validateDecl d
validateInstr (InstrStmt stmt) = validateStmt stmt
validateInstr (InstrBlock is) = validateLocally is

----------------------------------- PROGRAM -----------------------------------
validateProg :: Program -> StateT Context IO ()
validateProg (Prog is) = validateMultInstr is

