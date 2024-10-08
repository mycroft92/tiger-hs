{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
    runFile
    ) where


import Parser (parse)
import qualified Lexer as L
import qualified Data.ByteString.Lazy.Char8 as BS
import TypeChecking (runTypeChecker)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

runFile :: String -> IO Int
runFile s = do
    contents <- readFile s
    case (L.runAlex (BS.pack contents) parse) of
        Left err  -> putStr (show err) >> return 1
        Right exp -> do
            print exp
            out <- runTypeChecker exp  
            case out of
              Left errs -> mapM_ (putStr . show) errs >> return 1
              Right expty -> print expty >> return 0
