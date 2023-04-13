{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Data.Text (Text, pack, toLower, intercalate, unwords)
import AstTypes
import Control.Monad.State (State, get, gets, put)


-- data RenderState = RenderState
--     { indent :: Text
--     , negated :: Bool -- To prevent double negation from becoming a comment "--"
--     , rendered :: Text
--     }

-- newRenderState = RenderState
--     { indent = ""
--     , negated = False
--     , rendered = ""
--     }

-- renderLiteral :: Literal -> Text
-- renderLiteral l =
--     return $ case l of
--         IntLiteral x -> pack $ show x
--         StrLiteral x -> "\"" <> x <> "\""
--         BoolLiteral x -> toLower $ pack $ show x


renderLiteral :: Literal -> Text
renderLiteral l = case l of
    IntLiteral x -> pack $ show x
    StrLiteral x -> "\"" <> x <> "\""
    BoolLiteral x -> toLower $ pack $ show x

-- renderExpr :: Expr -> State RenderState Text
-- renderExpr ex = do
--     rs <- get
--     return $ indent rs <> case ex of 
--         LitExpr e        -> renderLiteral e
--         IdentifierExpr e -> e
--         IfExpr c e1 e2   -> do
            

renderExpr :: Text -> Expr -> Text
renderExpr inden ex = inden <> renderExpr' ex
  where
    renderExpr' :: Expr -> Text
    renderExpr' (LitExpr e) = renderLiteral e
    renderExpr' (IdentifierExpr e) = e
    renderExpr' (IfExpr b c1 c2) = "if "
        <> renderExpr "" b    <> " then" <> nl
        <> renderExpr "\t" c1 <> nl <> "else" <> nl
        <> renderExpr "\t" c2 <> nl <> "end"
    renderExpr' (LetExpr defs e) = "(function()" <> nl
        <> intercalate nl (map (renderDefinition "\t") defs) <> nl
        <> "\treturn (" <> renderExpr "" e <> ")" <> nl
        <> "end)()"
    renderExpr' (ApplyExpr e args) = "(" <> renderExpr "" e <> ")(" 
        <> intercalate ", " (map (renderExpr "") args) <> ")"
    renderExpr' (NegateExpr e) = "-" <> renderExpr "" e
    renderExpr' (BinExpr op e1 e2) = renderExpr "" e1 <> " " <> 
        case op of
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
        <> " " <> renderExpr "" e2
    renderExpr' (LambdaExpr args e) = "function(" <> Data.Text.unwords args <> ")" <> nl
        <> renderExpr "\t" e
        <> nl <> "end"
    renderExpr' (CaseExpr c cases) = "-" --TODO
    -- renderExpr _ = "-"

    nl = "\n" <> inden




-- TODO:
-- * Likely need to maintain state, for i.e
-- * double negate (ATM will double -, which will comment!)
-- renderExpr :: Expr -> Text
-- renderExpr x = case x of
--     LitExpr e -> renderLiteral e
--     IdentifierExpr e -> e
--     IfExpr c e1 e2 -> "if " <> renderExpr c <> " then " <>
--         renderExpr e1 <> " else " <> renderExpr e2 <> " end"
--     ApplyExpr e1 args -> "(" <> renderExpr e1 <> ")(" <>
--         intercalate ", " (Prelude.map renderExpr args) <> ")"
--     NegateExpr e -> "-" <> renderExpr e
--     BinExpr op e1 e2 -> renderExpr e1 <> " " <> case op of
--         Add -> "+"
--         Sub -> "-"
--         Mul -> "*"
--         Div -> "/"
--         Dollar -> "+" -- TODO
--         Compose -> "+" -- TODO
--         And -> "and"
--         Or -> "or"
--         Less -> "<"
--         LessEqual -> "<="
--         Greater -> ">"
--         GreaterEqual -> ">="
--         EqualTo -> "=="
--         NotEqualTo -> "~="
--         <> " " <> renderExpr e2
--     LambdaExpr args e -> "function(" <> Data.Text.unwords args <> ")\n\t"
--         <> renderExpr e
--         <> "\nend"
--     CaseExpr c cases -> "-" --TODO

-- -- TODO
renderPattern :: Pattern -> Text
renderPattern p = case p of
    WildcardPattern -> ""
    NamePattern n -> n
    LiteralPattern l -> renderLiteral l
    ConstructorPattern t ps -> ""

renderDefinition :: Text -> Definition -> Text
renderDefinition inden d = case d of
    ValueDefinition vd -> case vd of
        TypeAnnotation _ _ -> ""
        NameDefinition iden args e -> if not (Prelude.null args) then
            inden <> "function " <> iden 
            <> "(" <> intercalate ", " (Prelude.map renderPattern args) <> ")\n"
            <> renderExpr "\t" e
            <> "\nend"
        else
            inden <> "local " <> iden <> " = " <> renderExpr "" e
    DataDefinition {} -> ""
    TypeSynonym _ _ -> ""

renderAST :: AST -> Text
renderAST ast = intercalate "\n\n" (Prelude.map (renderDefinition "") definitions)
  where
    definitions = functions ast






