module TypeChecking where

import Env  -- ValueEnv, TypeEvnv
import Semantics
import AST
import Translate 
import Errors (Errors(TypeCheckError))
import Data.Foldable (foldrM)

type ExpTy         = (Translate.Exp, Semantics.Ty)
type TypeChecker a = Either Errors a

_checkInt          :: ExpTy -> Bool
_checkInt ((), INT) = True
_checkInt _         = False

_checkString          :: ExpTy -> Bool
_checkString ((), STRING) = True
_checkString _           = False

transExp            :: ValueEnv -> TypeEnv -> AST.Exp -> TypeChecker ExpTy
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

_checkArithOp       :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkArithOp left right = 
    if _checkInt left && _checkInt right
    then Right $ ((), INT)
    else Left $ TypeCheckError $ "Expected INT operands for arithmetic operation, but got " ++ show (snd left) ++ " and " ++ show (snd right)

_checkEqualityOp    :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkEqualityOp left right =
    if snd left == snd right
    then Right $ ((), INT)
    else Left $ TypeCheckError $ "Type mismatch in equality operation: " ++ show (snd left) ++ " vs " ++ show (snd right)

_checkComparisonOp  :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkComparisonOp left right
    | _checkInt left && _checkInt right       = Right $ ((),INT)
    | _checkString left && _checkString right = Right $ ((),INT)
    | otherwise = Left $ TypeCheckError $ "Invalid types for comparison: " ++ show (snd left) ++ " vs " ++ show (snd right)

_checkLogicalOp     :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkLogicalOp left right =
    if _checkInt left && _checkInt right
    then Right $ ((),INT)
    else Left $ TypeCheckError $ "Expected INT operands for logical operation, but got " ++ show (snd left) ++ " and " ++ show (snd right)

transVar            :: ValueEnv -> TypeEnv -> AST.Var -> TypeChecker ExpTy
transVar venv tenv var = undefined

_transFunctionCall  :: ValueEnv -> TypeEnv -> String -> [AST.Exp] -> Range -> TypeChecker ExpTy
_transFunctionCall venv tenv funcName args range = undefined

_transUnaryOp       :: ValueEnv -> TypeEnv -> AST.Exp -> Range -> TypeChecker ExpTy
_transUnaryOp venv tenv exp range = undefined

_transRecordCreation :: ValueEnv -> TypeEnv -> String -> [Field] -> Range -> TypeChecker ExpTy
_transRecordCreation venv tenv tyId fields range = undefined

_transSequence      :: ValueEnv -> TypeEnv -> [AST.Exp] -> Range -> TypeChecker ExpTy
_transSequence venv tenv exps range = undefined

_transAssignment    :: ValueEnv -> TypeEnv -> AST.Var -> AST.Exp -> Range -> TypeChecker ExpTy
_transAssignment venv tenv var exp range = undefined

_transIfThenElse    :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Maybe AST.Exp -> Range -> TypeChecker ExpTy
_transIfThenElse venv tenv condExp thenExp elseExp range = undefined

_transWhile         :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transWhile venv tenv condExp bodyExp range = undefined

_transFor           :: ValueEnv -> TypeEnv -> String -> Bool -> AST.Exp -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transFor venv tenv varName escape loExp hiExp bodyExp range = undefined

_transBreak         :: Range -> TypeChecker ExpTy
_transBreak range = undefined

_transLet           :: ValueEnv -> TypeEnv -> [AST.Dec] -> AST.Exp -> Range -> TypeChecker ExpTy
_transLet ven ten decl exp _ = do
    (venv,tenv) <- foldrM (\d (v,t)-> transDec v t d) (ven,ten) decl
    transExp venv tenv exp

_transArrayCreation :: ValueEnv -> TypeEnv -> String -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_transArrayCreation venv tenv typeName sizeExp initExp range = undefined

transDec            :: ValueEnv -> TypeEnv -> AST.Dec -> TypeChecker (ValueEnv, TypeEnv)
transDec venv tenv (VarDec name escape (Just typ) exp range) = undefined
    {-(_,expty) <- transExp venv tenv exp-}
    {-ifM -}
transDec venv tenv (VarDec name escape Nothing exp range)    = do
    (_,expty) <- transExp venv tenv exp
    let venv' = enter venv name (VarEntry expty) in
        return (venv', tenv)
transDec venv tenv (TypeDec typeDecs range)                  = undefined
transDec venv tenv (FunctionDec funDecs range)               = undefined
    
cycleCheck          :: TypeEnv -> String -> [String] -> TypeChecker ()
cycleCheck tenv name visited
    | name `elem` visited = Left $ TypeCheckError $ "Cyclic type declaration detected: " ++ unwords (reverse (name:visited))
    | otherwise = case look tenv name of
        Just (NAME _ (Just (NAME nextName _))) -> cycleCheck tenv nextName (name:visited)
        _ -> return ()

transTy             :: TypeEnv -> AST.Typ -> Semantics.Ty
transTy tenv (NameTy name r)     = undefined
transTy tenv (RecordTy fields r) = undefined
transTy tenv (ArrayTy name r)    = undefined

