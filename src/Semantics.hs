module Semantics where
-- These are semantics (unlike AST which models syntax)
type Uniq = Int

data Ty = INT
    | STRING
    | REC ![(String,Ty)] Uniq -- needs to be unique, can I make it a hash?
    | ARRAY Ty Uniq --- needs to be unique
    | NIL
    | UNIT
    | NAME String (Maybe Ty) deriving (Show, Eq, Ord)  -- Recursive types need the 'maybe' to be filled in later

--findField :: 

data EnvEntry 
    = VarEntry 
        { ty :: !Ty 
        }
    | FunEntry 
        { formals :: ![Ty]
        , result  :: !Ty 
        } deriving (Show, Eq, Ord)

