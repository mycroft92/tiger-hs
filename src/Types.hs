module Types where
    newtype Uniq = Int

    data Ty = INT
        | STRING
        | REC [(String,Ty)] Uniq -- needs to be unique, can I make it a hash?
        | ARRAY Ty Uniq --- needs to be unique
        | NIL
        | UNIT
        | NAME String (Maybe Ty)