module Semantics where
-- These are semantics (unlike AST which models syntax)
import Data.List (nub)
type Uniq = Int

data Ty = INT
    | STRING
    | REC ![(String,Ty)] Uniq -- needs to be unique, can I make it a hash?
    | ARRAY Ty Uniq --- needs to be unique
    | NIL
    | UNIT
    | NAME String (Maybe Ty) deriving (Show, Eq, Ord)  -- Recursive types need the 'maybe' to be filled in later

findField :: [(String,Ty)] -> String -> Maybe Ty
findField ls name = find ls name
    where
        find ((n,ty):xs) name = if name == n then Just ty else find xs name
        find [] name = Nothing

memField :: [(String,Ty)] -> String -> Bool
memField ls name = name `elem` map fst ls

checkNoDuplicates :: [(String,Ty)] -> Bool
checkNoDuplicates ls = length ls == length (nub . map fst $ ls)

data EnvEntry 
    = VarEntry 
        { ty :: !Ty 
        }
    | FunEntry 
        { formals :: ![Ty]
        , result  :: !Ty 
        } deriving (Show, Eq, Ord)

