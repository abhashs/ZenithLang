{-# LANGUAGE OverloadedStrings #-}
module TyperSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
-- import Data.Text (pack)

import AstTypes
import Typer

spec :: Spec
spec = parallel $ describe "Typer" $ do

    context "when inferring types" $ do
        it "should type infer a number literal" $
            testTI (LitExpr (IntLiteral 1)) `shouldReturn` Right IntT
        it "should type infer a string literal" $
            testTI (LitExpr (StrLiteral "foo")) `shouldReturn` Right StringT

        it "should not type infer an unbound identifier" $
            testTI (IdentifierExpr "foo") `shouldReturn` Left "unbound variable: foo"

        it "should type infer a bound identifier" $ 
            testTIWithEnv 
                (Map.singleton "foo" (Scheme [] IntT))
                (IdentifierExpr "foo")
            `shouldReturn`
            Right IntT

        it "should type infer (bound) expression application" $
            testTIWithEnv
                (Map.singleton "foo" (Scheme [] (IntT :-> IntT)))
                (ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 1)])
            `shouldReturn`
            Right IntT
        
        it "should not unify incorrect type expression application" $
            testTIWithEnv
                (Map.singleton "foo" (Scheme [] (IntT :-> BoolT :-> IntT)))
                (ApplyExpr 
                    (ApplyExpr 
                        (IdentifierExpr "foo")
                        [LitExpr (IntLiteral 1)])
                    [LitExpr (IntLiteral 2)])
            `shouldReturn`
            Left "types do not unify: BoolT vs. IntT\n in ApplyExpr (ApplyExpr (IdentifierExpr \"foo\") [LitExpr (IntLiteral 1)]) [LitExpr (IntLiteral 2)]"

        it "should type infer partially applied curried expression application" $
            testTIWithEnv
                (Map.singleton "foo" (Scheme [] (IntT :-> BoolT :-> IntT)))
                (ApplyExpr
                    (IdentifierExpr "foo")
                    [LitExpr (IntLiteral 1)])
            `shouldReturn`
            Right (BoolT :-> IntT)

        it "should type infer a negate expression" $
            testTI (NegateExpr (LitExpr (IntLiteral 1)))
            `shouldReturn`
            Right IntT

            
        it "should type infer a binary operation expression" $
            testTI (BinExpr Add 
                (LitExpr (IntLiteral 1))
                (LitExpr (IntLiteral 2)))
            `shouldReturn`
            Right IntT
        
        it "should type infer lambda expressions" $
            testTIWithEnv 
                (Map.singleton "foo" (Scheme [] (IntT :-> IntT)))
                (LambdaExpr ["x"] (ApplyExpr (IdentifierExpr "foo") [IdentifierExpr "x"]))
            `shouldReturn`
            Right (IntT :-> IntT)
            
