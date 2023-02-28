module Main where

import Parser
import Data.Text (pack)

main :: IO ()
main = putStrLn $ readExpr (pack "True")
