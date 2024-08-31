module Env where

    import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
    import Data.Map.Strict as Map ( Map, empty, lookup, insert, member)
    import AST (Exp (..))
    import Semantics (Ty(..), EnvEntry (..))
    import Data.Map as M (foldrWithKey)

    data Env a = Env {
        e_values  :: IORef (Map.Map String a),
        enclosing :: IORef (Maybe (Env a)) -- stack of envs, outermost env is nested the innermost
    }
    

    type ValueEnv = (Env EnvEntry) --- this env is a mapping from variables to their types
    type TypeEnv  = (Env Ty)       --- this env is from types to types 

    printEnv :: Show a => Env a -> IO String
    printEnv e = do
        ev  <- readIORef (e_values e)
        enc <- readIORef (enclosing e)
        case enc of
            Nothing -> do
                let x = foldrWithKey (\k v acc -> show k ++":"++ show v ++ ", "++ acc) "" ev in
                    print x >> return x
            Just e' -> do
                p <- printEnv e'
                let x = foldrWithKey (\k v acc -> show k ++":"++ show v ++ ", "++ acc) "" ev in 
                    do 
                        print $ "\n\t"++x
                        return $ p ++ "\n\t"++ x


    createEnv :: IO (Env a)
    createEnv = do
        eval <- newIORef Map.empty
        enc  <- newIORef Nothing 
        return $ Env eval enc

    createChildEnv :: (Env a) -> IO (Env a)
    createChildEnv env = do
        env' <- newIORef Map.empty
        enc  <- newIORef $ Just env
        return $ Env env' enc
    
    define :: String -> a -> (Env a) -> IO ()
    define x val env =   modifyIORef' (e_values env) (Map.insert x val)


