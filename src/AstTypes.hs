module AstTypes where
import Data.Text

data Literal
    = IntLiteral Int
    | StrLiteral Text
    | BoolLiteral Bool
    deriving (Eq, Ord, Show)

data Expr
    = LitExpr Literal
    | IdentifierExpr Text
    | LetExpr [Definition] Expr
    | IfExpr Expr Expr Expr
    | ApplyExpr Expr [Expr]
    | NegateExpr Expr
    | BinExpr BinOp Expr Expr
    | LambdaExpr [Text] Expr
    | CaseExpr Expr [(Pattern, Expr)]
    deriving (Eq, Show)

data BinOp =
    Add | Sub | Mul | Div | Dollar | Compose | And | Or |
    Less | LessEqual | Greater | GreaterEqual | EqualTo |
    NotEqualTo
    deriving (Eq, Show)

data Pattern
    = WildcardPattern
    | NamePattern Text
    | LiteralPattern Literal
    | ConstructorPattern Text [Pattern]
    deriving (Eq, Show)

infixr 2 :->
data Type
    = StringT
    | IntT
    | BoolT
    | CustomType Text [Type]
    | TVar Text
    | Type :-> Type
    deriving (Eq, Show)

-- data Kind = Star | Kfn Kind Kind
--     deriving (Eq, Show)

-- infixr 2 :->
-- data Type
--     = TVar TypeVar
--     | TCon TypeConstructor
--     | Type :-> Type
--     | TGen Int
--     deriving (Eq, Show)

-- data TypeVar = TypeVar Text Kind
--     deriving (Eq, Show)

-- data TypeConstructor = Tycon Text Kind
--     deriving (Eq, Show)


data Definition
    = ValueDefinition ValueDefinition
    | DataDefinition Text [Text] [ConstructorDefinition]
    | TypeSynonym Text Type
    deriving (Eq, Show)

data ConstructorDefinition
    = ConstructorDefinition Text [Type]
    deriving (Eq, Show)

data ValueDefinition
    = TypeAnnotation Text Type
    | NameDefinition Text [Pattern] Expr
    deriving (Eq, Show)

-- newtype AST = AST [Definition] deriving (Eq, Show)
newtype AST = AST
    { functions :: [Definition]
    }
    deriving (Eq, Show)
