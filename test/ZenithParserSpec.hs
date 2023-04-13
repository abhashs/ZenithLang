{-# LANGUAGE OverloadedStrings #-}
module ZenithParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
-- import Parser
import ZenithParser
import AstTypes
import Text.Megaparsec (parse)
-- import Text.Megaparsec.Debug (dbg)

spec :: Spec
spec = parallel $ describe "Parser" $ do
    context "when parsing whitespace and comments" $ do
        it "shoudld parse a comment" $
            parse comment "" "--comment" `shouldParse` ()
        it "should parse a block comment" $
            parse blockComment "" "{- comment -}" `shouldParse` ()
        it "should parse an indented newline" $
            parse spacing "" "\n\t   " `shouldParse` ()

    context "when parsing booleans" $ do
        it "should parse True" $
            parse literal "" "True" `shouldParse` BoolLiteral True
        it "should parse False" $
            parse literal "" "False" `shouldParse` BoolLiteral False

    context "when parsing strings" $ do
        it "should parse an empty string" $
            parse literal "" "\"\"" `shouldParse` StrLiteral ""
        it "should parse an arbitrary string" $
            parse literal "" "\"arbitrary\"" `shouldParse` StrLiteral "arbitrary"
        it "should parse an alphanumeric string" $
            parse literal "" "\"ab123\"" `shouldParse` StrLiteral "ab123"
        -- it "should parse an escaped characters" $
        --     parse stringParser "" "\"\\\"" `shouldParse` StringLiteral "\"\\\""

    context "when parsing numbers" $ do
        it "should parse a digit" $
            parse literal "" "1" `shouldParse` IntLiteral 1
        it "should parse multiple digits" $
            parse literal "" "10" `shouldParse` IntLiteral 10


    context "when parsing identifiers" $ do
        it "should parse a letter" $
            parse identifier "" "a" `shouldParse` "a"
        it "should parse a word" $
            parse identifier "" "foo" `shouldParse` "foo"
        it "should parse alphanumeric" $
            parse identifier "" "a1" `shouldParse` "a1"
        it "should parse underscores" $
            parse identifier "" "a_1" `shouldParse` "a_1"
        it "should not parse keywords" $
            parse identifier "" `shouldFailOn` "if"

    context "when parsing atoms" $ do
        it "should parse a number" $
            parse atom "" "1" `shouldParse` LitExpr (IntLiteral 1)
        it "should parse a boolean" $
            parse atom "" "False" `shouldParse` LitExpr (BoolLiteral False)
        it "should parse a string" $
            parse atom "" "\"string\"" `shouldParse` LitExpr (StrLiteral "string")
        it "should parse an identifier" $
            parse atom "" "foo" `shouldParse` IdentifierExpr "foo"
        it "should parse parentheses" $
            parse atom "" "(foo)" `shouldParse` IdentifierExpr "foo"

    context "when parsing expressions" $ do
        it "should parse an atomic expression" $
            parse expr "" "foo" `shouldParse` IdentifierExpr "foo"
        it "should parse an parenesed expression" $
            parse expr "" "(foo)" `shouldParse` IdentifierExpr "foo"
        it "should parse a negative unary expression" $
            parse unaryExpr "" "-\n\tfoo" `shouldParse` NegateExpr (IdentifierExpr "foo")
        it "should parse function application" $
            parse expr "" "foo 1" `shouldParse` ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 1)]
        it "should parse multiple function application" $
            parse expr "" "foo 1\n\t2" `shouldParse` ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 1), LitExpr (IntLiteral 2)]
        it "should parse a dollar binary function" $
            parse expr "" "1 $ 2" `shouldParse` BinExpr Dollar (LitExpr (IntLiteral 1)) (LitExpr (IntLiteral 2))  
        it "should parse a addition binary function" $
            parse expr "" "1 + 2" `shouldParse` BinExpr Add (LitExpr (IntLiteral 1)) (LitExpr (IntLiteral 2))  
        it "should parse a division binary function" $
            parse expr "" "1 / 2" `shouldParse` BinExpr Div (LitExpr (IntLiteral 1)) (LitExpr (IntLiteral 2))  
        it "should parse multiple binary functions" $
            parse expr "" "1\n\t*\n\t1 / 2 + 1" `shouldParse` 
                BinExpr Add 
                    (BinExpr Div 
                        (BinExpr Mul 
                            (LitExpr (IntLiteral 1))
                            (LitExpr (IntLiteral 1))) 
                        (LitExpr (IntLiteral 2)))
                    (LitExpr (IntLiteral 1))
        -- it "should parse let expressions" $
        --     parse expr "" "let x = True\n\tin x" `shouldParse`
        --         LetExpr 
        --             [ValueDefinition (NameDefinition "x" [] (LitExpr (BoolLiteral True)))]
        --             (IdentifierExpr "x")

        it "should parse if expressions" $
            parse expr "" "if True then True else False" `shouldParse` 
                IfExpr 
                    (LitExpr (BoolLiteral True))
                    (LitExpr (BoolLiteral True))
                    (LitExpr (BoolLiteral False))
        it "should parse complex if expressions" $
            parse expr "" "if foo 1 then\n\tfn 3 \"string\"\nelse\n{-comment-}\n\tfoo 10" `shouldParse`
                IfExpr
                    (ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 1)])
                    (ApplyExpr (IdentifierExpr "fn") [LitExpr (IntLiteral 3), LitExpr (StrLiteral "string")])
                    (ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 10)])
        it "should parse lambda expressions" $
            parse expr "" "\\x -> x" `shouldParse` LambdaExpr ["x"] (IdentifierExpr "x")
        it "should parse lambda expressions with multiple args" $
            parse expr "" "\\x y -> x" `shouldParse` LambdaExpr ["x", "y"] (IdentifierExpr "x")
        it "should parse complex expressions" $
            parse expr "" "if\n\tfn (foo 1 2) 3 then \\x -> x else \\x y -> x + y" `shouldParse`
                IfExpr 
                    (ApplyExpr (IdentifierExpr "fn") 
                        [ApplyExpr (IdentifierExpr "foo") [LitExpr (IntLiteral 1),LitExpr (IntLiteral 2)]
                        ,LitExpr (IntLiteral 3)])
                    (LambdaExpr ["x"] (IdentifierExpr "x"))
                    (LambdaExpr ["x","y"] (BinExpr Add (IdentifierExpr "x") (IdentifierExpr "y")))

    context "when parsing types" $ do
        it "should parse primitive types" $
            parse singleType "" "Number" `shouldParse` IntT
        it "should parse a function type" $
            parse typeExpr "" "Number -> Number" `shouldParse` (IntT :-> IntT)
        it "should parse a function type with type variables" $
            parse typeExpr "" "a -> Number" `shouldParse` (TVar "a" :-> IntT)
        it "should parse a type expression" $
            parse typeExpr "" "Number -> Number" `shouldParse` (IntT :-> IntT)

    context "when parsing definitions" $ do
        it "should parse a 2 argument function type annotation" $
            parse definition "" "foo :: String -> Number -> Number" `shouldParse`
                ValueDefinition (TypeAnnotation "foo" (StringT :-> IntT :-> IntT))
        -- it "should parse a basic definition" $
        --     parse ast "" "x :: Number\nx = 2" `shouldParse` 
        --         AST
        --             [ ValueDefinition (TypeAnnotation "x" IntT)
        --             , ValueDefinition (NameDefinition "x" [] (LitExpr (IntLiteral 2)))]
        it "should parse a basic definition" $
            parse ast "" "x :: Number\nx = 2" `shouldParse` 
                AST
                    { functions =
                        [ ValueDefinition (TypeAnnotation "x" IntT)
                        , ValueDefinition (NameDefinition "x" [] (LitExpr (IntLiteral 2)))
                        ]
                    }
        it "should parse a data definition" $
            parse ast "" "data X = X Number" `shouldParse`
                AST { functions =
                    [ DataDefinition "X" [] [ConstructorDefinition "X" [IntT]]]
                    }
        it "should parse multiple definitions" $
            parse ast "" "x :: Number\nx = 2\ny :: Number\ny = 2" `shouldParse` 
                AST { functions = 
                        [ ValueDefinition (TypeAnnotation "x" IntT)
                        , ValueDefinition (NameDefinition "x" [] (LitExpr (IntLiteral 2)))
                        , ValueDefinition (TypeAnnotation "y" IntT)
                        , ValueDefinition (NameDefinition "y" [] (LitExpr (IntLiteral 2)))]
                    }
        it "should parse multiple complex definitions" $
            parse ast "" "x :: Number\nx = fn\n\t2\n\ny :: Number\ny = 2\n\nz :: String\nz=\"foo\"" `shouldParse` 
                AST { functions = 
                        [ ValueDefinition (TypeAnnotation "x" IntT)
                        , ValueDefinition (NameDefinition "x" [] (ApplyExpr (IdentifierExpr "fn") [LitExpr (IntLiteral 2)]))
                        , ValueDefinition (TypeAnnotation "y" IntT)
                        , ValueDefinition (NameDefinition "y" [] (LitExpr (IntLiteral 2)))
                        , ValueDefinition (TypeAnnotation "z" StringT)
                        , ValueDefinition (NameDefinition "z" [] (LitExpr (StrLiteral "foo")))]
                    }


-- main :: IO ()
-- main = hspec parserSpec
