module Env where

    import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
    import Data.Map.Strict as Map ( Map, empty, lookup, insert, member)
    import AST (Exp (..))
    import Data.Map as M (foldrWithKey)

    data Env = Env {
        e_values  :: IORef (Map.Map String String),
        enclosing :: IORef (Maybe Env) -- stack of envs, outermost env is nested the innermost
    }


    printEnv :: Env -> IO String
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


    createEnv :: IO Env
    createEnv = do
        eval <- newIORef Map.empty
        enc  <- newIORef Nothing 
        return $ Env eval enc

    createChildEnv :: Env -> IO Env
    createChildEnv env = do
        env' <- newIORef Map.empty
        enc  <- newIORef $ Just env
        return $ Env env' enc
    
    define :: String -> String -> Env -> IO ()
    define x val env =   modifyIORef' (e_values env) (Map.insert x val)
