{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
    runFile
    ) where


import Parser (parse)
import Data.Text as T
import TypeChecking (runTypeChecker)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

runFile :: String -> IO Int
runFile s = do
    contents <- readFile s
    case (parse s (T.pack contents)) of
        Left err  -> putStr (show err) >> return 1
        Right exp -> 
            case runTypeChecker exp of
              Left errs -> mapM_ (putStr . show) errs >> return 1
              Right expty -> print expty >> return 0
