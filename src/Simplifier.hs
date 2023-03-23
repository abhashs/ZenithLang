{-# LANGUAGE OverloadedStrings #-}
module Simplifier where

import Data.Text (Text)
import AstTypes


-- Mainly just reduces expression applications to a curried form

simplifyExpr :: Expr -> Expr
simplifyExpr (LitExpr e) = LitExpr e
simplifyExpr (IdentifierExpr e) = IdentifierExpr e
-- simplifyExpr (IfExpr b c1 c2) = IfExpr (simplifyExpr b) (simplifyExpr c1) (simplifyExpr c2)
simplifyExpr (IfExpr b c1 c2) = 
    ApplyExpr (ApplyExpr (ApplyExpr (IdentifierExpr "cond") [b]) [c1]) [c2]
simplifyExpr (ApplyExpr e args) =
    foldl (\a v -> ApplyExpr a [simplifyExpr v]) (simplifyExpr e) args
simplifyExpr (NegateExpr e) = NegateExpr (simplifyExpr e)
simplifyExpr (BinExpr op e1 e2) = BinExpr op (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (LambdaExpr [] e) = LambdaExpr [] (simplifyExpr e)
simplifyExpr (LambdaExpr [a] e) = LambdaExpr [a] (simplifyExpr e)
simplifyExpr (LambdaExpr (a:as) e) = LambdaExpr [a] (simplifyExpr (LambdaExpr as e))
-- simplifyExpr (LambdaExpr [] e) = LetExpr [] (simplifyExpr e)
-- simplifyExpr (LambdaExpr [a] e) = 
--     LetExpr 
--         [ValueDefinition (NameDefinition ("KEYWORD_FN_"<>a) [NamePattern a] (simplifyExpr e))]
--         (IdentifierExpr ("KEYWORD_FN_"<>a) )
-- simplifyExpr (LambdaExpr (a:as) e) =
--     LetExpr
--         [ValueDefinition (NameDefinition ("KEYWORD_FN_"<>a) [NamePattern a] (simplifyExpr e))]
--         (simplifyExpr (LambdaExpr as e))
simplifyExpr (LetExpr [] e) = LetExpr [] (simplifyExpr e)
simplifyExpr (LetExpr [d] e) = LetExpr [d] (simplifyExpr e)
simplifyExpr (LetExpr (d:ds) e) = LetExpr [d] (simplifyExpr (LetExpr ds e))
simplifyExpr (CaseExpr e cases) = CaseExpr (simplifyExpr e) (map
    (\(p, e) -> (p, simplifyExpr e)) cases)


simplifyDefinition :: Definition -> Definition
simplifyDefinition (ValueDefinition d) = ValueDefinition $
    case d of
        TypeAnnotation ta t -> TypeAnnotation ta t
        NameDefinition nd args e -> NameDefinition nd args (simplifyExpr e)
simplifyDefinition (DataDefinition d c cd) = DataDefinition d c cd
simplifyDefinition (TypeSynonym d s) = TypeSynonym d s


simplify :: AST -> AST
simplify ast = AST (map simplifyDefinition (functions ast))
