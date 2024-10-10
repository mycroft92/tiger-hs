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
    | NAME String (Maybe Ty) deriving (Show, Ord)  -- Recursive types need the 'maybe' to be filled in later

instance Eq Ty where
    (REC _ k1)   == (REC _ k2)   = k1 == k2
    (REC _ _)    == NIL = True
    NIL          == (REC _ _) = True
    (ARRAY _ k1) == (ARRAY _ k2) = k1 == k2
    (NAME s1 t1) == (NAME s2 t2) = (s1 == s2) && (t1 ==t2)
    NIL          == NIL    = True
    STRING       == STRING = True
    INT          == INT    = True
    UNIT         == UNIT   = True
    _ == _ = False
isRecTy :: Ty -> Bool
isRecTy (REC _ _) = True
isRecTy _ = False

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

