module Main where

import ZenithParser
import Data.Text (pack)
import Text.Megaparsec (parse)

main :: IO ()
main = putStrLn $ case parse ast "" (pack "definition :: Number\ndefinition = 1") of
    Left x -> "Parse error"
    Right x -> show x
