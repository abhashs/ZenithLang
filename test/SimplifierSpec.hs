{-# LANGUAGE OverloadedStrings #-}
module SimplifierSpec (spec) where

import Test.Hspec
import Simplifier
import AstTypes


spec :: Spec
spec = parallel $ describe "Simplifier" $ do

    context "when simplifying expressions" $ do
        it "should reflect a literal expression" $
            simplifyExpr (LitExpr (IntLiteral 1))
                `shouldBe` LitExpr (IntLiteral 1)
        it "should reflect an identifier expression" $
            simplifyExpr (IdentifierExpr "x")
                `shouldBe` IdentifierExpr "x"
        it "should reflect a basic if expression" $
            simplifyExpr (IfExpr (IdentifierExpr "x") (IdentifierExpr "y") (IdentifierExpr "z"))
                `shouldBe` IfExpr (IdentifierExpr "x") (IdentifierExpr "y") (IdentifierExpr "z")
        it "should reflect single expression application" $
            simplifyExpr (ApplyExpr (IdentifierExpr "x") [LitExpr (IntLiteral 1)])
                `shouldBe` ApplyExpr (IdentifierExpr "x") [LitExpr (IntLiteral 1)]
        it "should simplify multiple expression application" $
            simplifyExpr (ApplyExpr (IdentifierExpr "x")
                [ LitExpr (IntLiteral 1)
                , LitExpr (IntLiteral 2)
                ])
                `shouldBe`
                ApplyExpr
                    (ApplyExpr (IdentifierExpr "x") [LitExpr (IntLiteral 1)])
                    [LitExpr (IntLiteral 2)]
        it "should reflect a lambda expression" $
            simplifyExpr (LambdaExpr ["x"] (IdentifierExpr "x"))
                `shouldBe` LambdaExpr ["x"] (IdentifierExpr "x")
        it "should reflect nested expressions" $
            simplifyExpr (IfExpr
                            (BinExpr EqualTo
                                (IdentifierExpr "x")
                                (IdentifierExpr "y"))
                            (LitExpr (IntLiteral 1))
                            (ApplyExpr
                                (IdentifierExpr "z")
                                [IdentifierExpr "x", IdentifierExpr "y"]))
                `shouldBe`
            simplifyExpr (IfExpr
                            (BinExpr EqualTo
                                (IdentifierExpr "x")
                                (IdentifierExpr "y"))
                            (LitExpr (IntLiteral 1))
                            (ApplyExpr
                                (ApplyExpr 
                                    (IdentifierExpr "z")
                                    [IdentifierExpr "x"])
                                [IdentifierExpr "y"]))

    context "when simplifying definitions" $ do
        it "should reflect a ValueDefinitiion" $
            simplifyDefinition (ValueDefinition (TypeAnnotation "fn" IntT))
                `shouldBe` ValueDefinition (TypeAnnotation "fn" IntT)

    context "when simplifying ASTs" $ do
        it "should reflect an AST" $
            simplify (AST { functions =
                    [ ValueDefinition (TypeAnnotation "fn" IntT)
                    , ValueDefinition (NameDefinition "fn" [] (IdentifierExpr "x"))
                    ]})
                `shouldBe`
                    AST { functions =
                            [ ValueDefinition (TypeAnnotation "fn" IntT)
                            , ValueDefinition (NameDefinition "fn" [] (IdentifierExpr "x"))
                            ]
                        }


