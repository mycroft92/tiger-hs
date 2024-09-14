module Env where

import Data.Map.Strict as Map (Map, empty, lookup, insert, member, fromList)
import AST (Exp (..))
import Semantics (Ty(..), EnvEntry (..))
import Data.Map as M (foldrWithKey)

type Env a    = (Map.Map String a)
type ValueEnv = (Env EnvEntry) -- this env is a mapping from variables to their types
type TypeEnv  = (Env Ty)       -- this env is from types to types

printEnv :: Show a => Env a -> IO ()
printEnv env = M.foldrWithKey printEntry (return ()) env
  where
    printEntry key value acc = do
      putStrLn $ key ++ " -> " ++ show value
      acc

emptyEnv :: Env a
emptyEnv = Map.empty

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.fromList [("int",INT), ("string", STRING), ("nil",NIL)]

addKey :: Env (Maybe a) -> String -> Env (Maybe a)
addKey env k = Map.insert k Nothing env

enter :: Env a -> String -> a -> Env a
enter env k v = Map.insert k v env 

look :: Env a -> String -> Maybe a
look env key = Map.lookup key env
