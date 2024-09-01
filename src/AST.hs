module AST where
data Pos = Pos { line:: Int, col:: Int} deriving (Ord, Eq)
data Range = Range {start:: Pos, stop:: Pos} deriving (Ord, Eq)

data Var = SimpleVar String Range | FieldVar Var String Range | SubscriptVar Var Exp Range deriving (Ord, Eq, Show)

data Binop = Plus | Minus | Times | Divide | Eq | Neq | Lt | Le  | Gt | Ge | LAnd | LOr deriving (Ord, Eq, Show)
data Field = Field {name:: String, value:: Exp, range :: Range} deriving (Ord, Eq, Show)

-- data RecField = DecField String Bool String Range deriving (Show)
-- instance Rangers DecField where
--     getRange (DecField _ _ _ r) =r

data FunDec = FunDec {fname:: String, params:: [RecField], result :: Maybe String, body:: Exp, frange :: Range} deriving (Ord, Eq, Show)
data TypeD  = TypeD String Typ Range deriving (Ord, Eq, Show)
data Typ    = NameTy String Range | RecordTy [RecField] Range | ArrayTy String Range deriving (Ord, Eq, Show)

-- name, escaping, type, range
data RecField = RecField String Bool String Range  deriving (Ord, Eq, Show)

data Dec =
    FunctionDec [FunDec] Range
    -- name, escaping, Maybe type, init, range
    | VarDec String Bool (Maybe String) Exp Range
    | TypeDec [TypeD] Range  deriving (Ord, Eq, Show)

errorDec :: Range -> Dec
errorDec = VarDec "error" False Nothing (NilExp (Range (Pos 0 0) (Pos 0 0)))

data Exp =
    VarExp Var
    | NilExp Range
    | IntExp Int Range
    | StringExp String Range
    | CallExp String [Exp] Range
    | UnopExp  Exp Range -- neg expression
    | BinopExp Exp Binop Exp Range
    -- Type name, constructor list, range
    | RecordExp String [Field] Range
    | SeqExp [Exp] Range
    | AssignExp Var Exp Range
    | IfExp Exp Exp (Maybe Exp) Range
    | WhileExp  Exp Exp Range
    -- var name, escaping, lo-hi, body, range
    | ForExp String Bool Exp Exp Exp Range
    | BreakExp Range
    | LetExp [Dec] Exp Range
    -- type, size, init, range
    | ArrayExp String Exp Exp Range deriving (Ord, Eq, Show)

squash :: Exp -> Exp
squash a@(SeqExp es r)
    | length es == 1  = head es
    | otherwise       = SeqExp (map squash es) r
squash a@(VarExp _)   = a
squash a@(NilExp _)   = a
squash a@(IntExp _ _) = a
squash a@(StringExp _ _) = a
squash (CallExp s es r)  = CallExp s (map squash es) r
squash (UnopExp e r)     = UnopExp (squash e) r
squash (BinopExp e1 op e2 r)  = BinopExp (squash e1) op (squash e2) r
squash a@(RecordExp {})       = a
squash (AssignExp v e r)      = AssignExp v (squash e) r
squash (IfExp c t f r)        = IfExp (squash c) (squash t)  (squash <$> f) r
squash (WhileExp c b r)       = WhileExp (squash c) (squash b) r
squash (ForExp s b c e1 e2 r) = ForExp s b (squash c) (squash e1) (squash e2) r
squash a@(BreakExp {})        = a
squash (LetExp ds e r)        = LetExp ds (squash e) r
squash (ArrayExp s e1 e2 r)   = ArrayExp s (squash e1) (squash e2) r
        

instance Show Pos where
    show (Pos l c) = "("++show l++","++show c ++")"

instance Show Range where
    show (Range s e) = show s ++ "-" ++ show e

class Rangers c where
    getRange :: c -> Range
    getEnd   :: c -> Pos
    getEnd c = stop $ getRange c
    getStart :: c -> Pos
    getStart c = start $ getRange c
    {-# MINIMAL getRange #-}

instance Rangers Field where
    getRange (Field _ _ r) =r

instance Rangers Var where
    getRange (SimpleVar _ r)  = r
    getRange (FieldVar _ _ r) = r
    getRange (SubscriptVar _ _ r) = r

instance Rangers FunDec where
    getRange (FunDec _ _ _ _ r) =r

instance Rangers Typ where
    getRange (NameTy _ r)   = r
    getRange (RecordTy _ r) = r
    getRange (ArrayTy _ r)  = r

instance Rangers TypeD where
    getRange (TypeD _ _ r) =r

instance Rangers RecField where
    getRange (RecField _ _ _ r) = r

instance Rangers Dec where
    getRange (FunctionDec _ r)  = r
    getRange (VarDec _ _ _ _ r) = r
    getRange (TypeDec _ r)      = r

instance Rangers Exp where
    getRange (VarExp v) = getRange v
    getRange (NilExp r) = r
    getRange (IntExp _ r) = r
    getRange (StringExp _ r) = r
    getRange (CallExp _ _ r) = r
    getRange (UnopExp  _ r)  = r
    getRange (BinopExp _ _ _ r) = r
    getRange (RecordExp _ _ r)  = r
    getRange (SeqExp _ r)       = r
    getRange (AssignExp _ _ r)  = r
    getRange (IfExp _ _ _ r)    = r
    getRange (WhileExp  _ _ r)  = r
    getRange (ForExp _ _ _ _ _ r) = r
    getRange (BreakExp r)         = r
    getRange (LetExp _ _ r)       = r
    getRange (ArrayExp _ _ _ r)   = r
