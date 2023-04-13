{-# LANGUAGE OverloadedStrings #-}
module RendererSpec (spec) where

import Test.Hspec

import AstTypes
import Renderer

spec :: Spec
spec = parallel $ describe "Renderer" $ do

    context "when renderering literals" $ do
        it "should render a number literal" $
            renderLiteral (IntLiteral 1) `shouldBe` "1"
        it "should render a string literal" $
            renderLiteral (StrLiteral "string") `shouldBe` "\"string\""
        it "should render a boolean literal" $
            renderLiteral (BoolLiteral True) `shouldBe` "true"

    context "when renderering expressions" $ do
        it "should render an identifier" $
            renderExpr "" (IdentifierExpr "foo") `shouldBe` "foo"
        it "should render an if expression" $
            renderExpr ""
                (IfExpr
                    (LitExpr (BoolLiteral True))
                    (LitExpr (IntLiteral 1))
                    (LitExpr (IntLiteral 2)))
            `shouldBe`
                "if true then\n\t1\nelse\n\t2\nend"
        it "should render nested if expression" $
            renderExpr ""
                (IfExpr
                    (IdentifierExpr "a")
                    (IfExpr (IdentifierExpr "x") (IdentifierExpr "y") (IdentifierExpr "z"))
                    (IfExpr (IdentifierExpr "y") (IdentifierExpr "b") (IdentifierExpr "c")))
            `shouldBe`
                "if a then\n\
                \\tif x then\n\
                \\t\ty\n\
                \\telse\n\
                \\t\tz\n\
                \\tend\n\
                \else\n\
                \\tif y then\n\
                \\t\tb\n\
                \\telse\n\
                \\t\tc\n\
                \\tend\n\
                \end"
        it "should render let expressions" $
            renderExpr ""
                (LetExpr
                    [ ValueDefinition (NameDefinition "x" [] (LitExpr (IntLiteral 1)))
                    , ValueDefinition (NameDefinition "y" [] (LitExpr (IntLiteral 2)))
                    ]
                    (BinExpr Add (IdentifierExpr "x") (IdentifierExpr "y")))
            `shouldBe`
                "(function()\n\tlocal x = 1\n\tlocal y = 2\n\treturn (x + y)\nend)()"
        it "should render an expression application" $
            renderExpr ""
                (ApplyExpr (IdentifierExpr "foo")
                    [ LitExpr (BoolLiteral False)
                    , LitExpr (IntLiteral 10)
                    ])
            `shouldBe`
                "(foo)(false, 10)"
        it "should render a curried expression appplication" $
            renderExpr ""
                (ApplyExpr
                    (ApplyExpr (IdentifierExpr "foo")
                        [ LitExpr (BoolLiteral False) ])
                    [ LitExpr (IntLiteral 10) ])
            `shouldBe`
                "((foo)(false))(10)"
        it "should render a negation expression" $
            renderExpr "" (NegateExpr (LitExpr (IntLiteral 1))) `shouldBe` "-1"
        it "should render a binary expression" $
            renderExpr "" (BinExpr Add
                (LitExpr (IntLiteral 1))
                (LitExpr (IntLiteral 11)))
            `shouldBe`
                "1 + 11"
        it "should render a lambda expression" $
            renderExpr "" (LambdaExpr ["x"] (IdentifierExpr "x"))
            `shouldBe`
                "function(x)\n\tx\nend"
        it "should render a curried lambda expression with multiple args" $
            renderExpr "" (LambdaExpr ["x"] (LambdaExpr ["y"] (IdentifierExpr "x")))
            `shouldBe`
                "function(x)\n\tfunction(y)\n\t\tx\n\tend\nend"
        it "should render nested expressions" $ 
            renderExpr ""
            (IfExpr
                (ApplyExpr (IdentifierExpr "foo")
                    [ LitExpr (BoolLiteral False)
                    , LitExpr (IntLiteral 10)])
                (LitExpr (StrLiteral "bar"))
                (ApplyExpr (LambdaExpr ["x"] (IdentifierExpr "x"))
                    [ LitExpr (BoolLiteral True) ]))
            `shouldBe`
                "if (foo)(false, 10) then\n\t\"bar\"\nelse\n\t(function(x)\n\tx\nend)(true)\nend"

    context "when renderering definitions" $ do
        it "should render a variable definition" $
            renderDefinition "" (ValueDefinition
                (NameDefinition "x" [] (LitExpr (IntLiteral 1))))
            `shouldBe`
            "local x = 1"
        it "should render a function definition" $
            renderDefinition "" (ValueDefinition
                (NameDefinition "y" [NamePattern "arg"] (LitExpr (IntLiteral 1))))
            `shouldBe`
            "function y(arg)\n\t1\nend"

    context "when renderering asts" $ do
        it "should render a simple ast" $
            renderAST AST {
                functions = [ValueDefinition
                    (NameDefinition "y" [NamePattern "arg"] (LitExpr (IntLiteral 1)))]
                }
            `shouldBe`
            "function y(arg)\n\t1\nend"
        it "should render multiple definitions" $
            renderAST AST {
                functions = [ ValueDefinition (NameDefinition
                                "x" [] (LitExpr (IntLiteral 1)))
                            , ValueDefinition (NameDefinition
                                "y" [NamePattern "arg"] (LitExpr (IntLiteral 1)))
                            ]
                }
            `shouldBe`
            "local x = 1\n\nfunction y(arg)\n\t1\nend"
