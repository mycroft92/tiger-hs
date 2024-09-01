module TypeChecking where

import Env  -- ValueEnv, TypeEvnv
import Semantics
import AST
import Translate 
import Errors (Errors(TypeCheckError))
import Control.Monad (liftM2, unless)

type ExpTy                  = (Translate.Exp, Semantics.Ty)
type TypeChecker a          = Either Errors a

_checkInt                   :: ExpTy -> Bool
_checkInt ((), INT)         = True
_checkInt _                 = False

_checkString                :: ExpTy -> Bool
_checkString ((), STRING)   = True
_checkString _              = False

transExp                    :: ValueEnv -> TypeEnv -> AST.Exp -> TypeChecker ExpTy
transExp venv tenv (VarExp var)                   = transVar venv tenv var
transExp venv tenv (NilExp range)                 = Right $ ((), NIL)
transExp venv tenv (IntExp n range)               = Right $ ((), INT)
transExp venv tenv (StringExp str range)          = Right $ ((), STRING)
transExp venv tenv (CallExp func args range)      = _transFunctionCall venv tenv func args range
transExp venv tenv (UnopExp exp range)            = _transUnaryOp venv tenv exp range
transExp venv tenv (BinopExp left op right range) = do
    leftTy  <- transExp venv tenv left
    rightTy <- transExp venv tenv right
    case op of
        Plus   -> _checkArithOp leftTy rightTy
        Minus  -> _checkArithOp leftTy rightTy
        Times  -> _checkArithOp leftTy rightTy
        Divide -> _checkArithOp leftTy rightTy
        Eq     -> _checkEqualityOp leftTy rightTy
        Neq    -> _checkEqualityOp leftTy rightTy
        Lt     -> _checkComparisonOp leftTy rightTy
        Le     -> _checkComparisonOp leftTy rightTy
        Gt     -> _checkComparisonOp leftTy rightTy
        Ge     -> _checkComparisonOp leftTy rightTy
        LAnd   -> _checkLogicalOp leftTy rightTy
        LOr    -> _checkLogicalOp leftTy rightTy
transExp venv tenv (RecordExp tyid fields range)  = _transRecordCreation venv tenv tyid fields range
transExp venv tenv (SeqExp exps range)            = _transSequence venv tenv exps range
transExp venv tenv (AssignExp var exp range)      = _transAssignment venv tenv var exp range
transExp venv tenv (IfExp cond then' else' range) = _transIfThenElse venv tenv cond then' else' range
transExp venv tenv (WhileExp cond body range)     = _transWhile venv tenv cond body range
transExp venv tenv (BreakExp range)               = _transBreak range
transExp venv tenv (LetExp decs body range)       = _transLet venv tenv decs body range
transExp venv tenv (ArrayExp typ size init range) = _transArrayCreation venv tenv typ size init range
transExp venv tenv (ForExp var escape lo hi body range) = _transFor venv tenv var escape lo hi body range

_checkArithOp               :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkArithOp left right    = 
    if _checkInt left && _checkInt right
    then Right $ ((), INT)
    else Left $ TypeCheckError $ "Expected INT operands for arithmetic operation, but got " ++ show (snd left) ++ " and " ++ show (snd right)

_checkEqualityOp            :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkEqualityOp left right =
    if snd left == snd right
    then Right $ ((), INT)
    else Left $ TypeCheckError $ "Type mismatch in equality operation: " ++ show (snd left) ++ " vs " ++ show (snd right)

_checkComparisonOp          :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkComparisonOp left right
    | _checkInt left && _checkInt right       = Right $ ((),INT)
    | _checkString left && _checkString right = Right $ ((),INT)
    | otherwise = Left $ TypeCheckError $ "Invalid types for comparison: " ++ show (snd left) ++ " vs " ++ show (snd right)

_checkLogicalOp             :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkLogicalOp left right  =
    if _checkInt left && _checkInt right
    then Right $ ((),INT)
    else Left $ TypeCheckError $ "Expected INT operands for logical operation, but got " ++ show (snd left) ++ " and " ++ show (snd right)

-- Helper functions (to be implemented)
transVar                    :: ValueEnv -> TypeEnv -> AST.Var -> TypeChecker ExpTy
transVar                    = undefined

_transFunctionCall          :: ValueEnv -> TypeEnv -> String -> [AST.Exp] -> Range -> TypeChecker ExpTy
_transFunctionCall          = undefined

_transUnaryOp               :: ValueEnv -> TypeEnv -> AST.Exp -> Range -> TypeChecker ExpTy
_transUnaryOp               = undefined

_transRecordCreation        :: ValueEnv -> TypeEnv -> String -> [Field] -> Range -> TypeChecker ExpTy
_transRecordCreation        = undefined

_transSequence              :: ValueEnv -> TypeEnv -> [AST.Exp] -> Range -> TypeChecker ExpTy
_transSequence              = undefined

_transAssignment            :: ValueEnv -> TypeEnv -> AST.Var -> AST.Exp -> Range -> TypeChecker ExpTy
_transAssignment            = undefined

_transIfThenElse            :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Maybe AST.Exp -> Range -> TypeChecker ExpTy
_transIfThenElse            = undefined

_transWhile                 :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transWhile                 = undefined

_transFor                   :: ValueEnv -> TypeEnv -> String -> Bool -> AST.Exp -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transFor                   = undefined

_transBreak                 :: Range -> TypeChecker ExpTy
_transBreak                 = undefined

_transLet                   :: ValueEnv -> TypeEnv -> [AST.Dec] -> AST.Exp -> Range -> TypeChecker ExpTy
_transLet ven ten decl exp r = transExp venv tenv exp
    where
        (venv,tenv) = foldr (\d (v,t)-> transDec v t d) (ven,ten) decl 

_transArrayCreation         :: ValueEnv -> TypeEnv -> String -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transArrayCreation         = undefined

transDec                    :: ValueEnv -> TypeEnv -> AST.Dec -> TypeChecker (ValueEnv, TypeEnv)
transDec venv tenv (VarDec name escape jt exp r) = do
    (_, expTy) <- transExp venv tenv exp
    case jt of
        Just typeName -> do
            declaredTy <- case look tenv typeName of
                Just t  -> return t
                Nothing -> throwError $ TypeCheckError $ "Undefined type: " ++ typeName
            if expTy == declaredTy
                then return (enter venv name (VarEntry expTy), tenv)
                else throwError $ TypeCheckError $ "Type mismatch in variable declaration: expected " ++ show declaredTy ++ ", but got " ++ show expTy
        Nothing -> return (enter venv name (VarEntry expTy), tenv)

transDec venv tenv (TypeDec typeDecs) = do
    let addTypePlaceholder (env, names) (TypeD name _ _) = 
            (enter env name (NAME name Nothing), name : names)
    let (tenv', typeNames) = foldr addTypePlaceholder (tenv, []) typeDecs
    
    tenv'' <- foldM (\env (TypeD name ty _) -> do
        actualType <- transTy env ty
        return $ enter env name actualType) tenv' typeDecs
    
    -- Check for cycles in type declarations
    forM_ typeNames $ \name -> do
        cycleCheck tenv'' name []
    
    return (venv, tenv'')

transDec venv tenv (FunctionDec funDecs) = do
    let addFunHeader (venv', names) (FunDec name params resultTy _ _) = do
            paramTys <- mapM (\(RecField _ _ typeName _) -> 
                                case look tenv typeName of
                                    Just t  -> return t
                                    Nothing -> throwError $ TypeCheckError $ "Undefined parameter type: " ++ typeName) params
            resTy <- case resultTy of
                        Just tyName -> case look tenv tyName of
                                        Just t  -> return t
                                        Nothing -> throwError $ TypeCheckError $ "Undefined return type: " ++ tyName
                        Nothing     -> return UNIT
            return (enter venv' name (FunEntry paramTys resTy), name : names)
    
    (venv', funNames) <- foldM addFunHeader (venv, []) funDecs
    
    venv'' <- foldM (\venv (FunDec name params resultTy body _) -> do
        let (FunEntry paramTys resultTy') = fromJust $ look venv' name
        let addParam (env, i) (RecField paramName _ _ _) = 
                (enter env paramName (VarEntry (paramTys !! i)), i + 1)
        let (bodyVenv, _) = foldl addParam (venv', 0) params
        (_, bodyTy) <- transExp bodyVenv tenv body
        if bodyTy == resultTy'
            then return venv
            else throwError $ TypeCheckError $ "Function " ++ name ++ " body type mismatch: expected " ++ show resultTy' ++ ", but got " ++ show bodyTy
        ) venv' funDecs
    
    return (venv'', tenv)

-- Helper function to check for cycles in type declarations
cycleCheck :: TypeEnv -> String -> [String] -> TypeChecker ()
cycleCheck tenv name visited
    | name `elem` visited = throwError $ TypeCheckError $ "Cyclic type declaration detected: " ++ unwords (reverse (name:visited))
    | otherwise = case look tenv name of
        Just (NAME _ (Just (NAME nextName _))) -> cycleCheck tenv nextName (name:visited)
        _ -> return ()

transTy                     :: TypeEnv -> AST.Typ -> Semantics.Ty
transTy                     = undefined
