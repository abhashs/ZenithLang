{-# LANGUAGE OverloadedStrings #-}
module TyperSpec (spec) where

import Test.Hspec
-- import qualified Data.Map as Map
-- import Data.Text (pack)

import AstTypes
import Typer

spec :: Spec
spec = parallel $ describe "Typer" $ do

    context "when type inferring" $ do
        it "should type infer a number literal" $
            testTI (LitExpr (IntLiteral 1)) `shouldReturn` Right IntT
            
            
            
