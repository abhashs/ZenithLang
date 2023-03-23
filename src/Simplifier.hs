{-# LANGUAGE OverloadedStrings #-}
module Simplifier where

-- Mainly just reduces expression applications to a curried form

import AstTypes

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
simplifyExpr (LambdaExpr [] e) = LambdaExpr [] e
simplifyExpr (LambdaExpr [a] e) = LambdaExpr [a] e
simplifyExpr (LambdaExpr (a:as) e) = LambdaExpr [a] (simplifyExpr (LambdaExpr as e))
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
