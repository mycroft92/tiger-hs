module Semantics where
    -- These are semantics (unlike AST which models syntax)
    type Uniq = Int

    data Ty = INT
        | STRING
        | REC [(String,Ty)] Uniq -- needs to be unique, can I make it a hash?
        | ARRAY Ty Uniq --- needs to be unique
        | NIL
        | UNIT
        | NAME String (Maybe Ty) deriving (Show)  -- Recursive types need the 'maybe' to be filled in later