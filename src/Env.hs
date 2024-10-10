module Env where

import Data.Map as Map (Map, empty, lookup, insert, member, fromList, foldrWithKey)
import AST (Exp (..))
import Semantics (Ty(..), EnvEntry (..))

type Env a    = (Map.Map String a)
type ValueEnv = (Env EnvEntry) -- this env is a mapping from variables to their types
type TypeEnv  = (Env Ty)       -- this env is from types to types

printEnv :: Show a => Env a -> IO ()
printEnv env = Map.foldrWithKey printEntry (return ()) env
  where
    printEntry key value acc = do
      putStrLn $ key ++ " -> " ++ show value
      acc

emptyValueEnv :: ValueEnv
emptyValueEnv = Map.fromList [("getchar", FunEntry {formals = [], result = STRING}),("ord", FunEntry {formals = [STRING], result = INT}), ("print", FunEntry {formals = [STRING], result = NIL}), ("chr", FunEntry {formals = [INT], result = STRING})]

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.fromList [("int",INT), ("string", STRING), ("nil",NIL)]




addKey :: Env (Maybe a) -> String -> Env (Maybe a)
addKey env k = Map.insert k Nothing env

enter :: Env a -> String -> a -> Env a
enter env k v = Map.insert k v env 

look :: Env a -> String -> Maybe a
look env key = Map.lookup key env
