module AST where
    data Pos = Pos { line:: Int, col:: Int} deriving (Eq)
    data Range = Range {start:: Pos, stop:: Pos} deriving (Eq)

    data Var = SimpleVar String Range | FieldVar Var String Range | SubscriptVar Var Exp Range deriving (Show)

    data Binop = Plus | Minus | Times | Divide | Eq | Neq | Lt | Le  | Gt | Ge | LAnd | LOr deriving (Show)
    data Field = Field {name:: String, value:: Exp, range :: Range} deriving (Show)

    -- data RecField = DecField String Bool String Range deriving (Show)
    -- instance Rangers DecField where
    --     getRange (DecField _ _ _ r) =r

    data FunDec = FunDec {fname:: String, params:: [RecField], result :: Maybe String, body:: Exp, frange :: Range} deriving (Show)
    data TypeD  = TypeD String Typ Range deriving (Show)
    data Typ    = NameTy String Range | RecordTy [RecField] Range | ArrayTy String Range deriving (Show)

    -- name, escaping, type, range
    data RecField = RecField String Bool String Range  deriving (Show)

    data Dec = 
        FunctionDec [FunDec] Range
        -- name, escaping, Maybe type, init, range
        | VarDec String Bool (Maybe String) Exp Range
        | TypeDec [TypeD] Range deriving (Show)
    
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
        | ArrayExp String Exp Exp Range deriving (Show)

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
        getRange (AssignExp _ _ r)= r
        getRange (IfExp _ _ _ r)    = r
        getRange (WhileExp  _ _ r)  = r
        getRange (ForExp _ _ _ _ _ r) = r
        getRange (BreakExp r)         = r
        getRange (LetExp _ _ r)       = r
        getRange (ArrayExp _ _ _ r)   = r
