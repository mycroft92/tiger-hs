{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
    runFile
    ) where


import Parser (parse)
import Data.Text as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"

runFile :: String -> IO Int
runFile s = do
    contents <- readFile s
    case (parse s (T.pack contents)) of
        Left err  -> print err >> return 1
        Right exp -> print exp >> return 0