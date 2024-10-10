
{-# LANGUAGE FlexibleContexts #-}
module TypeChecking (runTypeChecker) where
import Env  -- ValueEnv, TypeEnv
import Semantics
import AST
import Translate 
import Errors (Errors(TypeCheckError))
import Data.Foldable (foldlM, foldrM)
import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get,put),
      MonadTrans(lift))
import Control.Monad.State (State, 
    runState,
    MonadState(get,put),
    MonadTrans(lift))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
import Control.Monad (forM)
import Data.Bits (Bits(xor))
import qualified Data.Map as Map (map, toList, fromList)

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

runTypeChecker :: AST.Exp -> IO (Either [Errors] ExpTy)
runTypeChecker exp = do
    out <- runStateT (runExceptT (typeCheckExp emptyValueEnv emptyTypeEnv exp)) _initState 
    case out of
      (Left err,  TypeCheckerState errors _ _)   ->  return$  Left $ reverse (err:errors)
      (Right expty, TypeCheckerState errors _ _) -> if length errors > 0 then return (Left (reverse errors)) else return (Right expty)

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
    else if ((isRecTy left && right == NIL) || (isRecTy right && left == NIL)) then return INT 
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
      Just (VarEntry ty) -> return ty
      Just _             -> throwError $ TypeCheckError $ "Expected a variable: "++ var ++" found a function instead"
      Nothing  -> throwError $ TypeCheckError $ "Undefined identifier: "++var++" used at: "++show range

typeCheckVar venv tenv (FieldVar var field range) = do
    varTy <- typeCheckVar venv tenv var
    case varTy of
        REC fields _ ->
            case lookup field fields of
                Just fieldTy -> 
                    case fieldTy of
                      NAME fty (Just fty') -> return fty'
                      NAME fty Nothing    -> throwError $ TypeCheckError $ "Field " ++ field ++ " has unknown type: "++show fty++" at: " ++ show range
                      _ -> return fieldTy
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
_typeCheckFunctionCall venv tenv funcName args range = 
    case look venv funcName of
      Just (FunEntry formals result) -> 
          if length formals == length args then 
            mapM_ checkArg (zip args formals) >> return result
          else throwError $ TypeCheckError $ "Function call: "++funcName++" at: "++ show range++ " has arity: "++show (length formals) ++ " received: "++ show (length args)
      Just _  -> throwError $ TypeCheckError $ "Identifier: "++funcName ++ " at: "++ show range ++ " is not a function!"
      Nothing -> throwError $ TypeCheckError $ "Function" ++ funcName ++ " at: "++show range ++ " is not defined!"
    where
      checkArg :: (AST.Exp,Semantics.Ty) -> TypeChecker ()
      checkArg (exp, formal) = do
          expty <- typeCheckExp venv tenv exp
          if expty == formal then return () else throwError $ TypeCheckError $ "In function call: "++funcName++" at: "++ show range++ " arg: "++show exp ++ " expected to have: "++ show formal++ " but has:"++show expty

_typeCheckUnaryOp     :: ValueEnv -> TypeEnv -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckUnaryOp venv tenv exp range = do
    expty <- typeCheckExp venv tenv exp
    case expty of
      INT -> return expty
      ty  -> throwError $ TypeCheckError $ "Expr: "++ show exp++" at: "++show range ++" has type: "++show ty ++ " while it is expected to have type INT"

_typeCheckRecordCreation :: ValueEnv -> TypeEnv -> String -> [Field] -> Range -> TypeChecker ExpTy
_typeCheckRecordCreation venv tenv tyId fields range = 
    case look tenv tyId of
      Just rty@(REC paramList _) -> 
         -- Check that all fields of the record are present 
         if verifyFields paramList fields then
            mapM (\(Field name value range) -> 
                case findField paramList name of
                 -- Just (NAME ty' (Just ty)) -> do
                 --     fty <- typeCheckExp venv tenv value
                 --     if fty == ty then return () else throwError $ TypeCheckError $ "Record field type mistmatch: "++ name++"at: "++show range ++ " expected: "++ show ty ++ " found: "++ show fty
                  Just ty -> do
                      fty <- typeCheckExp venv tenv value
                      if fty == ty then return () else throwError $ TypeCheckError $ "Record field type mistmatch: "++ name++"at: "++show range ++ " expected: "++ show ty ++ " found: "++ show fty
                  Nothing -> throwError (TypeCheckError $ "Record Type: "++ tyId ++ " at: "++show range ++" has no field named: "++name)) fields >> return rty
         else throwError $ TypeCheckError $ "Record Type initialization: "++ tyId ++ " at: "++show range ++" doesnot have all the fields. Expected: "++ concatMap show paramList
      _ -> throwError $ TypeCheckError $ "Type: "++ tyId ++ " at: "++show range ++" is not a valid record type."
    where
        verifyFields :: [(String,Semantics.Ty)] -> [Field] -> Bool
        verifyFields ps fs = foldl (\acc (Field name _ _) -> acc && memField ps name) True fs 
      

_typeCheckSequence    :: ValueEnv -> TypeEnv -> [AST.Exp] -> Range -> TypeChecker ExpTy
_typeCheckSequence venv tenv exps range = 
    listLast <$> mapM (\exp -> typeCheckExp venv tenv exp) exps
      where 
            listLast [x] = x
            listLast (_:xs) = listLast xs
            listLast [] = NIL

_typeCheckAssignment  :: ValueEnv -> TypeEnv -> AST.Var -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckAssignment venv tenv var exp range = do
    varTy <- typeCheckVar venv tenv var
    expTy <- typeCheckExp venv tenv exp
    if varTy == expTy
        then return NIL
        else throwError $ TypeCheckError $ "Type mismatch in assignment at " ++ show range ++ ": variable has type " ++ show varTy ++ ", but expression has type " ++ show expTy

_typeCheckIfThenElse  :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Maybe AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckIfThenElse venv tenv condExp thenExp elseExp range = do
    condTy <- typeCheckExp venv tenv condExp
    if not (_checkInt condTy)
        then throwError $ TypeCheckError $ "Condition in if-then-else must be of type INT at " ++ show range
        else do
            thenTy <- typeCheckExp venv tenv thenExp
            case elseExp of
                Just elseE -> do
                    elseTy <- typeCheckExp venv tenv elseE
                    if thenTy == elseTy
                        then return thenTy
                        else throwError $ TypeCheckError $ "Types in then and else branches must match at " ++ show range
                Nothing -> return thenTy

_typeCheckWhile       :: ValueEnv -> TypeEnv -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckWhile venv tenv condExp bodyExp range = do
    condTy <- typeCheckExp venv tenv condExp
    if not (_checkInt condTy)
        then throwError $ TypeCheckError $ "Condition in while loop must be of type INT at " ++ show range
        else do
            _breakUp
            bodyTy <- typeCheckExp venv tenv bodyExp
            _breakDown
            return bodyTy

_typeCheckFor         :: ValueEnv -> TypeEnv -> String -> Bool -> AST.Exp -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckFor venv tenv varName escape loExp hiExp bodyExp range = do
    loTy <- typeCheckExp venv tenv loExp
    hiTy <- typeCheckExp venv tenv hiExp
    if not (_checkInt loTy && _checkInt hiTy)
        then throwError $ TypeCheckError $ "Loop bounds must be of type INT at " ++ show range
        else do
            let venv' = enter venv varName (VarEntry INT)
            _breakUp
            bodyTy <- typeCheckExp venv' tenv bodyExp
            _breakDown
            return bodyTy

_typeCheckBreak       :: Range -> TypeChecker ExpTy
_typeCheckBreak range = do
    isInLoop <- _breakCheck
    if isInLoop
        then return NIL
        else throwError $ TypeCheckError $ "Break statement outside of loop at " ++ show range

_typeCheckLet         :: ValueEnv -> TypeEnv -> [AST.Dec] -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckLet ven ten decl exp _ = do
    (venv, tenv) <- foldlM (\(v, t) d-> typeCheckDec v t d) (ven, ten) decl
    typeCheckExp venv tenv exp

correctEnv :: TypeEnv -> TypeChecker TypeEnv
correctEnv tenv = do
    l <- mapM handle (Map.toList tenv)
    return (Map.fromList l)
    where
        --handle :: (Monad m) => (String, Ty) -> m (String, Ty)
        handle (k,(NAME name _)) = case look tenv name of
                                       -- change this to NAME type?
                                       Just ty -> return (k,ty)
                                       Nothing -> throwError $ TypeCheckError $ "Var "++k ++" has Type: "++name ++ " not defined!"
        handle (v,(REC fields k))      = do
            flds <- mapM handle fields
            liftIO $ (mapM_ print flds)
            return (v,(REC flds k))
        handle (v, ARRAY ty u) = do
            (_,ty') <- handle (v,ty)
            return $ (v, ARRAY ty' u) 
        handle ty = return ty


_typeCheckArrayCreation :: ValueEnv -> TypeEnv -> String -> AST.Exp -> AST.Exp -> Range -> TypeChecker ExpTy
_typeCheckArrayCreation venv tenv typeName sizeExp initExp range = do
    case look tenv typeName of
        Just (ARRAY elemTy _) -> do
            sizeTy <- typeCheckExp venv tenv sizeExp
            initTy <- typeCheckExp venv tenv initExp
            if not (_checkInt sizeTy)
                then throwError $ TypeCheckError $ "Array size must be of type INT at " ++ show range
                else if initTy /= elemTy
                    then throwError $ TypeCheckError $ "Array initialization type mismatch at " ++ show range
                    else return $ ARRAY elemTy (length $ show elemTy)
        Just _ -> throwError $ TypeCheckError $ "Type " ++ typeName ++ " is not an array type at " ++ show range
        Nothing -> throwError $ TypeCheckError $ "Undefined type " ++ typeName ++ " at " ++ show range

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
    tenv' <- foldlM (\tenv' (TypeD name _ _)-> 
        return $ enter tenv' name (NAME name Nothing)) tenv typeDecs
    (venv''', tenv''') <- foldlM (\(venv'', tenv'') typeD-> typeCheckTypeD venv'' tenv'' typeD) (venv, tenv') typeDecs
    tenv'''' <- correctEnv tenv'''
    liftIO $ print tenv''''
    return (venv''', tenv'''')

typeCheckDec venv tenv (FunctionDec funDecs range) = do
    -- mutually recursive function support (except cycle checks)
    venv' <- foldlM (\venv' fundec-> _generateFunType tenv venv' fundec) venv funDecs
    -- Typecheck the bodies
    mapM (\fundec -> _typeCheckFunction tenv venv' fundec) funDecs
    return (venv', tenv)


typeCheckTypeD        :: ValueEnv -> TypeEnv -> TypeD -> TypeChecker (ValueEnv, TypeEnv)
typeCheckTypeD venv tenv (TypeD name typ range) = do
    actualType <- typeCheckTy tenv typ
    let newTenv = enter tenv name actualType
    cycleCheck newTenv name [] range
    return (venv, newTenv)

-- This is a much more liberal cycle check, in that it allows typealiases as long as there's no cycle
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
typeCheckTy tenv rec@(RecordTy fields r) = do
    fieldTypes <- mapM (\(RecField name _ typeName range) -> 
        case look tenv typeName of
            Just t  -> return (name, t)
            Nothing -> throwError $ TypeCheckError $ "Undefined type in record field: " ++ typeName ++ " " ++ show range) fields
    --Check for duplicate entries in record
    if checkNoDuplicates fieldTypes then 
        REC fieldTypes <$> _getUniq
    else
        throwError $ TypeCheckError $ "Duplicate field names within the record: "++ show rec
typeCheckTy tenv (ArrayTy name r) = 
    case look tenv name of
        Just t  -> ARRAY t <$> _getUniq
        Nothing -> throwError $ TypeCheckError $ "Undefined array element type: " ++ name ++ " " ++ show r
 
_generateFunType      :: TypeEnv -> ValueEnv -> FunDec -> TypeChecker ValueEnv
_generateFunType tenv venv (FunDec{fname=fn, fparams=ps, fresult=res, fbody=body, frange=rng}) = do
    case res of
      -- There is a return statement
      Just ty -> do
          case look tenv ty of
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
    venv' <- foldlM (\venv' p@(RecField n _ tyn range) -> enter venv' n . VarEntry <$> _typeCheckParam tenv p) venv ps
    rt    <- typeCheckExp venv' tenv body
    case (look tenv res) of
      Just ty -> if rt == ty then return () else throwError $ TypeCheckError $ "Return type mismatch in function: "++show fn++": "++show rng++ " given: "++ show ty ++" computed: "++show rt
      Nothing -> throwError $ TypeCheckError $ "Return type not found in function: "++show fn++": "++show rng++ " given: "++show res ++" computed: "++show rt

_typeCheckFunction tenv venv (FunDec{fname=fn, fparams=ps, fresult=Nothing, fbody=body, frange=rng}) = do
    -- add parameters to the value environment
    venv' <- foldlM (\venv' p@(RecField n _ tyn range)-> enter venv' n . VarEntry <$> _typeCheckParam tenv p) venv ps
    rt    <- typeCheckExp venv' tenv body
    if rt == NIL then return () else throwError $ TypeCheckError $ "Return type mismatch in function: "++show fn++": "++show rng++ " given: "++ show NIL ++" computed: "++show rt
    

_typeCheckParam       :: TypeEnv -> RecField -> TypeChecker Semantics.Ty
_typeCheckParam tenv (RecField fn _ tyn range) = 
    case (look tenv tyn) of
      Just ty -> return ty
      Nothing -> throwError $ TypeCheckError $ "Parameter "++fn++" has an unknown type: "++ tyn ++":"++show range
