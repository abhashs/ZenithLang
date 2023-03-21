{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Data.Text
import AstTypes


test :: Text
test = "test"

renderLiteral :: Literal -> Text
renderLiteral l = case l of
    IntLiteral x -> pack $ show x
    StrLiteral x -> "\"" <> x <> "\""
    BoolLiteral x -> toLower $ pack $ show x


-- TODO:
-- * Likely need to maintain state, for i.e
-- * double negate (ATM will double -, which will comment!)
renderExpr :: Expr -> Text
renderExpr x = case x of
    LitExpr e -> renderLiteral e
    IdentifierExpr e -> e
    IfExpr c e1 e2 -> "if " <> renderExpr c <> " then " <>
        renderExpr e1 <> " else " <> renderExpr e2 <> " end"
    ApplyExpr e1 args -> "(" <> renderExpr e1 <> ")(" <>
        intercalate ", " (Prelude.map renderExpr args) <> ")"
    NegateExpr e -> "-" <> renderExpr e
    BinExpr op e1 e2 -> renderExpr e1 <> " " <> case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Dollar -> "+" -- TODO
        Compose -> "+" -- TODO
        And -> "and"
        Or -> "or"
        Less -> "<"
        LessEqual -> "<="
        Greater -> ">"
        GreaterEqual -> ">="
        EqualTo -> "=="
        NotEqualTo -> "~="
        <> " " <> renderExpr e2
    LambdaExpr args e -> "function(" <> Data.Text.unwords args <> ")\n\t"
        <> renderExpr e
        <> "\nend"
    CaseExpr c cases -> "-" --TODO

-- TODO
renderPattern :: Pattern -> Text
renderPattern p = case p of
    WildcardPattern -> ""
    NamePattern n -> n
    LiteralPattern l -> renderLiteral l
    ConstructorPattern t ps -> ""

renderDefinition :: Definition -> Text
renderDefinition d = case d of
    ValueDefinition vd -> case vd of
        TypeAnnotation _ _ -> ""
        NameDefinition iden args e -> if not (Prelude.null args) then
            "function " <> iden <> "(" <> intercalate ", " (Prelude.map renderPattern args) <> ")\n\t"
                <> renderExpr e <> "\nend"
        else
            "local " <> iden <> " = " <> renderExpr e
    DataDefinition {} -> ""
    TypeSynonym _ _ -> ""

renderAST :: AST -> Text
renderAST ast = intercalate "\n\n" (Prelude.map renderDefinition definitions)
  where
    definitions = functions ast
    





