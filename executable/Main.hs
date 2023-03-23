{-# LANGUAGE OverloadedStrings #-}
module Main where

import ZenithParser
import Renderer
import Simplifier
import Data.Text (pack)
import Text.Megaparsec (parse)

main :: IO ()
main = putStrLn $ case parse ast "" code of
    Left x -> "Parse error"
    Right x -> show $ renderAST x
  where
    code =
        "test :: Number\n" 
        <> "test = if True then\n \tif True then\n \t\t12 \telse\n \t\t13\n else\n \t 1"

