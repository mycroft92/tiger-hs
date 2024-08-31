module Types where

data Ty
  = INT
  | STRING
  | RECORD [(String, Ty)] Unique
  | ARRAY Ty Unique
  | NIL
  | UNIT
  | NAME String (Maybe Ty)
  deriving (Eq, Ord, Show)

type Unique = Int

-- Add any other necessary types or functions here
