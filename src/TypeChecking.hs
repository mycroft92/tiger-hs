module TypeChecking where

import Env  -- ValueEnv, TypeEnv
import Semantics
import AST
import Translate 
import Errors (Errors(TypeCheckError))
import Data.Foldable (foldrM)
import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get,put),
      MonadTrans(lift))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)

-- type ExpTy            = (Translate.Exp, Semantics.Ty)
type ExpTy            = Semantics.Ty

data TypeCheckerState = TypeCheckerState {
      errors :: [Errors],
      unique :: Int,
      breakNest :: Int
    } deriving Show

type TypeChecker a    = ExceptT Errors (StateT TypeCheckerState IO) a

_checkInt             :: ExpTy -> Bool
_checkInt INT   = True
_checkInt _           = False

_checkString          :: ExpTy -> Bool
_checkString STRING = True
_checkString _        = False

_initState            :: TypeCheckerState
_initState            = TypeCheckerState {errors = [], unique = 0, breakNest = 0}

_getUniq              :: TypeChecker Int
_getUniq              = do
    st <- lift get
    let u = unique st + 1
    _ <- lift $ put (st{unique = u})
    return u

_addError             :: Errors -> TypeChecker ()
_addError e           = do
    st <- lift get
    let errs = e:errors st
    _ <- lift $ put (st{errors = errs})
    return ()

_breakUp :: TypeChecker ()
_breakUp = do
    st <- lift get
    lift $ put (st{breakNest = breakNest st +1}) 

_breakDown :: TypeChecker ()
_breakDown = do
    st <- lift get
    lift $ put (st{breakNest = breakNest st - 1})

_breakCheck :: TypeChecker Bool
_breakCheck = do
    st <- lift get
    if (breakNest st <= 0) then return False else return True 

typeCheckExp          :: ValueEnv -> TypeEnv -> AST.Exp -> TypeChecker ExpTy
typeCheckExp venv tenv (VarExp var)                   = typeCheckVar venv tenv var
typeCheckExp venv tenv (NilExp range)                 = return NIL
typeCheckExp venv tenv (IntExp n range)               = return INT
typeCheckExp venv tenv (StringExp str range)          = return STRING
typeCheckExp venv tenv (CallExp func args range)      = _typeCheckFunctionCall venv tenv func args range
typeCheckExp venv tenv (UnopExp exp range)            = _typeCheckUnaryOp venv tenv exp range
typeCheckExp venv tenv (BinopExp left op right range) = do
    leftTy  <- typeCheckExp venv tenv left
    rightTy <- typeCheckExp venv tenv right
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
typeCheckExp venv tenv (RecordExp tyid fields range)  = _typeCheckRecordCreation venv tenv tyid fields range
typeCheckExp venv tenv (SeqExp exps range)            = _typeCheckSequence venv tenv exps range
typeCheckExp venv tenv (AssignExp var exp range)      = _typeCheckAssignment venv tenv var exp range
typeCheckExp venv tenv (IfExp cond then' else' range) = _typeCheckIfThenElse venv tenv cond then' else' range
typeCheckExp venv tenv (WhileExp cond body range)     = _typeCheckWhile venv tenv cond body range
typeCheckExp venv tenv (BreakExp range)               = _typeCheckBreak range
typeCheckExp venv tenv (LetExp decs body range)       = _typeCheckLet venv tenv decs body range
typeCheckExp venv tenv (ArrayExp typ size init range) = _typeCheckArrayCreation venv tenv typ size init range
typeCheckExp venv tenv (ForExp var escape lo hi body range) = _typeCheckFor venv tenv var escape lo hi body range

_checkArithOp         :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkArithOp left right = 
    if _checkInt left && _checkInt right
    then return INT
    else throwError $ TypeCheckError $ "Expected INT operands for arithmetic operation, but got " ++ show left ++ " and " ++ show right

_checkEqualityOp      :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkEqualityOp left right =
    if left == right
    then return INT
    else throwError $ TypeCheckError $ "Type mismatch in equality operation: " ++ show left ++ " vs " ++ show right

_checkComparisonOp    :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkComparisonOp left right
    | _checkInt left && _checkInt right       = return INT
    | _checkString left && _checkString right = return INT
    | otherwise = throwError $ TypeCheckError $ "Invalid types for comparison: " ++ show left ++ " vs " ++ show right

_checkLogicalOp       :: ExpTy -> ExpTy -> TypeChecker ExpTy
_checkLogicalOp left right =
    if _checkInt left && _checkInt right
    then return INT
    else throwError $ TypeCheckError $ "Expected INT operands for logical operation, but got " ++ show left ++ " and " ++ show right

typeCheckVar          :: ValueEnv -> TypeEnv -> AST.Var -> TypeChecker ExpTy
typeCheckVar venv tenv (SimpleVar var range) = 
    case look venv var of
      Just (VarEntry {ty=ty}) -> return ty
      Nothing  -> throwError $ TypeCheckError $ "Undefined identifier: "++var++" used at: "++show range

typeCheckVar venv tenv (FieldVar var field range) = do
    varTy <- typeCheckVar venv tenv var
    case varTy of
        REC fields _ ->
            case lookup field fields of
                Just fieldTy -> return fieldTy
                Nothing -> throwError $ TypeCheckError $ "Field " ++ field ++ " not found in record: "++show var++" at: " ++ show range
        ty -> throwError $ TypeCheckError $ "Trying to access field " ++ field ++ " of non-record: "++show var++" type: " ++ show ty ++"at: " ++ show range

typeCheckVar venv tenv (SubscriptVar var index range) = do
    varTy <- typeCheckVar venv tenv var
    indexTy <- typeCheckExp venv tenv index
    case varTy of
        ARRAY elemTy _ ->
            if _checkInt indexTy
            then return elemTy
            else throwError $ TypeCheckError $ "Array index must be an integer at: " ++ show range
        _ -> throwError $ TypeCheckError $ "Trying to index a non-array type at: " ++ show range 

_typeCheckFunctionCall :: ValueEnv -> TypeEnv -> String -> [AST.Exp] -> Range -> TypeChecker ExpTy
_typeCheckFunctionCall venv tenv funcName args range = undefined

_typeCheckUnaryOp     :: ValueEnv -> TypeEnv -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckUnaryOp venv tenv exp range = do
    expty <- typeCheckExp venv tenv exp
    case expty of
      INT -> return expty
      ty  -> throwError $ TypeCheckError $ "Expr: "++ show exp++" at: "++show range ++" has type: "++show ty ++ " while it is expected to have type INT"

_typeCheckRecordCreation :: ValueEnv -> TypeEnv -> String -> [Field] -> Range -> TypeChecker ExpTy
_typeCheckRecordCreation venv tenv tyId fields range = 
    case look tenv tyId of
      Just (REC paramList _) -> do

      _ -> throwError $ TypeCheckError $ "Type: "++ tyId ++ " at: "++show range ++" is not a valid record type."
      

_typeCheckSequence    :: ValueEnv -> TypeEnv -> [AST.Exp] -> Range -> TypeChecker ExpTy
_typeCheckSequence venv tenv exps range = undefined

_typeCheckAssignment  :: ValueEnv -> TypeEnv -> AST.Var -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckAssignment venv tenv var exp range = undefined

_typeCheckIfThenElse  :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Maybe AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckIfThenElse venv tenv condExp thenExp elseExp range = undefined

_typeCheckWhile       :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckWhile venv tenv condExp bodyExp range = undefined

_typeCheckFor         :: ValueEnv -> TypeEnv -> String -> Bool -> AST.Exp -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckFor venv tenv varName escape loExp hiExp bodyExp range = undefined

_typeCheckBreak       :: Range -> TypeChecker ExpTy
_typeCheckBreak range = undefined

_typeCheckLet         :: ValueEnv -> TypeEnv -> [AST.Dec] -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckLet ven ten decl exp _ = do
    (venv, tenv) <- foldrM (\d (v, t) -> typeCheckDec v t d) (ven, ten) decl
    typeCheckExp venv tenv exp

_typeCheckArrayCreation :: ValueEnv -> TypeEnv -> String -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckArrayCreation venv tenv typeName sizeExp initExp range = undefined

typeCheckDec          :: ValueEnv -> TypeEnv -> AST.Dec -> TypeChecker (ValueEnv, TypeEnv)
typeCheckDec venv tenv (VarDec name escape (Just typ) exp range) = do
    expty  <- (typeCheckExp venv tenv exp) `catchError` (\e -> _addError e >> return NIL)
    case look tenv typ of
      Just typ' -> 
          if typ' == expty then 
                   do
                     let venv' = enter venv name (VarEntry expty)
                     return (venv', tenv)
          else
            do
                _addError (TypeCheckError $ "Type mismatch in decl: " ++ show name ++ " given: "++ show typ' ++ " computed: "++ show expty ++ ":"++ show range)
                return (venv,tenv)
      Nothing -> do
          _addError (TypeCheckError $ "Unknown type: "++ show typ ++" used in decl: "++show name ++" :"++show range++ " expected: "++ show expty)
          return (venv,tenv)

typeCheckDec venv tenv (VarDec name escape Nothing exp range) = do
    expty <- (typeCheckExp venv tenv exp) `catchError` (\e -> _addError e >> return NIL)
    let venv'  = enter venv name (VarEntry expty)
    return (venv', tenv)

typeCheckDec venv tenv (TypeDec typeDecs range) = do
    -- make the headers first, then process the RHS of each definition.
    -- This is required to support mutually recursive types
    tenv' <- foldrM (\(TypeD name _ _) tenv' -> 
        return $ enter tenv' name (NAME name Nothing)) tenv typeDecs
    foldrM (\typeD (venv'', tenv'') -> typeCheckTypeD venv'' tenv'' typeD) (venv, tenv') typeDecs

typeCheckDec venv tenv (FunctionDec funDecs range) = do
    -- mutually recursive function support (except cycle checks)
    venv' <- foldrM (\fundec venv' -> _generateFunType tenv venv' fundec) venv funDecs
    -- Typecheck the bodies
    mapM (\fundec -> _typeCheckFunction tenv venv' fundec) funDecs
    return (venv', tenv)


typeCheckTypeD        :: ValueEnv -> TypeEnv -> TypeD -> TypeChecker (ValueEnv, TypeEnv)
typeCheckTypeD venv tenv (TypeD name typ range) = do
    actualType <- typeCheckTy tenv typ
    let newTenv = enter tenv name actualType
    cycleCheck newTenv name [] range
    return (venv, newTenv)

cycleCheck            :: TypeEnv -> String -> [String] -> Range -> TypeChecker ()
cycleCheck tenv name visited range
    | name `elem` visited = throwError $ TypeCheckError $ "Cyclic type declaration detected: " ++ show range ++ ": " ++ unwords (reverse (name:visited))
    | otherwise = case look tenv name of
        Just (NAME _ (Just (NAME nextName _))) -> cycleCheck tenv nextName (name:visited) range
        _ -> return ()

typeCheckTy           :: TypeEnv -> AST.Typ -> TypeChecker Semantics.Ty
typeCheckTy tenv (NameTy name r) = 
    case look tenv name of
        Just t  -> return t
        Nothing -> throwError $ TypeCheckError $ "Undefined type: " ++ name
typeCheckTy tenv (RecordTy fields r) = do
    fieldTypes <- mapM (\(RecField name _ typeName range) -> 
        case look tenv typeName of
            Just t  -> return (name, t)
            Nothing -> throwError $ TypeCheckError $ "Undefined type in record field: " ++ typeName ++ " " ++ show range) fields
    --Check for duplicate entries in record
    return $ REC fieldTypes (length fieldTypes)
typeCheckTy tenv (ArrayTy name r) = 
    case look tenv name of
        Just t  -> return $ ARRAY t (length $ show t)
        Nothing -> throwError $ TypeCheckError $ "Undefined array element type: " ++ name ++ " " ++ show r
 
_generateFunType      :: TypeEnv -> ValueEnv -> FunDec -> TypeChecker ValueEnv
_generateFunType tenv venv (FunDec{fname=fn, fparams=ps, fresult=res, fbody=body, frange=rng}) = do
    case res of
      -- There is a return statement
      Just ty -> do
          case (look tenv ty) of
            Nothing -> throwError $ TypeCheckError $ "undefined return type: "++ ty ++" in fn: "++fn++" "++show rng
            Just rt -> do
                -- find the param types
                paramTypes <- mapM (\param -> _typeCheckParam tenv param) ps
                return $ enter venv fn (FunEntry {result= rt, formals=paramTypes})
      Nothing -> do
          paramTypes <- mapM (\param -> _typeCheckParam tenv param) ps
          -- I think the result type could be UNIT as well?
          return $ enter venv fn (FunEntry {result= NIL, formals=paramTypes})

_typeCheckFunction    :: TypeEnv -> ValueEnv -> FunDec -> TypeChecker ()
_typeCheckFunction tenv venv (FunDec{fname=fn, fparams=ps, fresult=(Just res), fbody=body, frange=rng}) = do
    -- add parameters to the value environment
    venv' <- foldrM (\p@(RecField n _ tyn range) venv'-> enter venv' n . VarEntry <$> _typeCheckParam tenv p) venv ps
    rt    <- typeCheckExp venv tenv body
    case (look tenv res) of
      Just ty -> if rt == ty then return () else throwError $ TypeCheckError $ "Return type mismatch in function: "++show fn++": "++show rng++ " given: "++ show ty ++" computed: "++show rt
      Nothing -> throwError $ TypeCheckError $ "Return type not found in function: "++show fn++": "++show rng++ " given: "++show res ++" computed: "++show rt

_typeCheckFunction tenv venv (FunDec{fname=fn, fparams=ps, fresult=Nothing, fbody=body, frange=rng}) = do
    -- add parameters to the value environment
    venv' <- foldrM (\p@(RecField n _ tyn range) venv'-> enter venv' n . VarEntry <$> _typeCheckParam tenv p) venv ps
    rt    <- typeCheckExp venv tenv body
    if rt == NIL then return () else throwError $ TypeCheckError $ "Return type mismatch in function: "++show fn++": "++show rng++ " given: "++ show NIL ++" computed: "++show rt
    

_typeCheckParam       :: TypeEnv -> RecField -> TypeChecker Semantics.Ty
_typeCheckParam tenv (RecField fn _ tyn range) = 
    case (look tenv tyn) of
      Just ty -> return ty
      Nothing -> throwError $ TypeCheckError $ "Parameter "++fn++" has an unknown type: "++ tyn ++":"++show range
