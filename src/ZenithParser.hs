{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- {-# LANGUAGE LambdaCase #-}

module ZenithParser where

import Data.Void
import Data.Functor (void)
import Control.Applicative(Alternative(..), optional, liftA2)
import Data.Text
import Data.List (foldl')
import Text.Megaparsec.Char (char, string, space, digitChar, letterChar, alphaNumChar, space1, lowerChar, upperChar, symbolChar, punctuationChar)

import qualified Data.Char as Char
import Numeric.Natural (Natural)
import Text.Megaparsec (Parsec, oneOf, noneOf, try, between, lookAhead, notFollowedBy, manyTill, eof, sepBy)
import Control.Monad.Combinators.Expr (makeExprParser)
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)
import Data.Function ((&))

import AstTypes
import Text.Read (Lexeme(String))

type Parser = Parsec Void Text

--Utility
stringParse :: String -> Parser Text
stringParse s = string $ pack s

parseThenSpace :: String -> Parser Text
parseThenSpace s = stringParse s <* spacing1

-- Whitespace parsing

-- ignore :: Text -> Parser ()

voidChar :: Char -> Parser ()
voidChar t = void (char t)

anyCommentChar :: Parser ()
anyCommentChar = void alphaNumChar <|> void symbolChar <|>  void punctuationChar <|> space1

blockComment :: Parser ()
blockComment = void (blockStart *> manyTill anyCommentChar (try blockEnd))
  where
    blockStart = stringParse "{-"
    blockEnd = stringParse "-}"

comment :: Parser ()
comment = void (stringParse "--" *> manyTill anyCommentChar (try (voidChar '\n' <|> eof)))

basicSpace :: Parser ()
basicSpace = voidChar ' ' <|> voidChar '\t' <|> blockComment

basicSpaces :: Parser ()
basicSpaces = void $ many basicSpace

endOfLine :: Parser ()
-- endOfLine = void $ many (voidChar '\n' <|> voidChar ';' <|> comment)
endOfLine = void $ some (voidChar '\n' <|> voidChar ';' <|> comment)

indentAfterEndOfLine :: Parser ()
indentAfterEndOfLine = void (endOfLine *> some basicSpace)

anySpacing :: Parser ()
anySpacing = void . many $ (basicSpace <|> comment <|> voidChar '\n')

-- Optional spacing with ident after any newline
spacing :: Parser ()
spacing = void . many $ (basicSpace <|> try indentAfterEndOfLine)

-- Mandatory spacing with an indent required after any newline
spacing1 :: Parser ()
spacing1 = void . some $ (basicSpace <|> indentAfterEndOfLine)
-- spacing = (void (some indentAfterEndOfLine) <|> multipleNewLines) <* basicSpaces
--   where
--     multipleNewLines = void (many endOfLine <* some indentAfterEndOfLine)
-- spacing = basicSpaces <* newlinesIndented <* basicSpaces
--   where
--     newlinesIndented = endOfLine <* void (many indentAfterEndOfLine)
-- spacing = basicSpaces <* newlinesIndented <* basicSpaces
--   where
--     newlinesIndented = void (some (voidChar ';')) <|> comment <|> void (many (voidChar '\n') <* voidChar '\t')

-- indentedSpacing :: Parser ()
-- indentedSpacing = basicSpaces <* endOfLine <* void (some indentAfterEndOfLine) <* basicSpaces
-- spacing = anySpacing' <* indentAfterEndOfLine
--   where
--     anySpacing' = void . many $ (basicSpace <|> comment)


-- indentedNewline :: Parser ()
-- indentedNewline = string (pack "\n\t") *> basicSpaces
-- indentedNewLine = 


blankLine :: Parser ()
blankLine = void (basicSpace *> many (char '\n'))

blankLines :: Parser ()
blankLines = void (many blankLine)

parensed :: Parser a -> Parser a
parensed p = char '(' *> spacing *> p <* spacing <* char ')'

--Atom parsers
keyword :: Parser ()
keyword = void (
    stringParse "in"
    <|> stringParse "let"
    <|> stringParse "if"
    <|> stringParse "then"
    <|> stringParse "else"
    <|> stringParse "data")

-- binFn :: Parser Tex
-- binFn = stringParse "+" <|> stringParse "-" <|> stringParse "*" <|> stringParse "/"

-- boolean :: Parser Expression
-- boolean =
--     (trueParser >> return (BooleanLiteral True)) <|>
--     (falseParser >> return (BooleanLiteral False))
--       where
--         trueParser = stringParse "True"
--         falseParser = stringParse "False"

-- number :: Parser Expression
-- number = do
--     x <- some digitChar
--     return $ IntegerLiteral $ read x

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    oneOf "\\\""

-- stringParser :: Parser Expression
-- stringParser = do
--     char '"'
--     x <- many $ escapedChars <|> noneOf "\"\\"
--     char '"'
--     return $ StringLiteral $ pack x

-- identifier :: Parser Expression
-- identifier = do
--     x <- letterChar <|> char '_'
--     xs <- many $ alphaNumChar <|> char '_'
--     return $ Identifier $ pack (x:xs)

anyIdentifier :: Parser Text
anyIdentifier = do
    x <- letterChar <|> char '_'
    xs <- many $ alphaNumChar <|> char '_'
    return $ pack (x:xs)

-- TODO: Review for performance
identifier :: Parser Text
identifier = notFollowedBy keyword *> anyIdentifier


lowercaseIdentifier :: Parser Text
lowercaseIdentifier = notFollowedBy keyword *> do
    x <- lowerChar <|> char '_'
    xs <- many $ alphaNumChar <|> char '_'
    return $ pack (x:xs)

typeIdentifier :: Parser Text
typeIdentifier = do
    x <- upperChar
    xs <- many $ alphaNumChar <|> char '_'
    return $ pack (x:xs)


-- atom :: Parser Expression
-- atom = number <|> boolean <|> stringParser <|> identifier

literal :: Parser Literal
literal = intLit <|> stringLit <|> boolLit
  where
    intLit = do
        x <- some digitChar
        return $ IntLiteral $ read x
    stringLit = do
        char '"'
        x <- many $ escapedChars <|> noneOf "\"\\"
        char '"'
        return $ StrLiteral $ pack x
    boolLit =
        (trueParser >> return (BoolLiteral True)) <|>
        (falseParser >> return (BoolLiteral False))
          where
            trueParser = stringParse "True"
            falseParser = stringParse "False"

-- Expression
-- binaryFunctionApplication :: Parser Expression
-- binaryFunctionApplication = do
--     symbol <- binFn
--     binFn *> space *> expr

unaryExpr :: Parser Expr
unaryExpr = negateExpr <|> appExpr
  where
    -- negateExpr = (char '-' *> spacing *> appExpr) & fmap NegateExpr
    negateExpr = fmap NegateExpr (char '-' *> spacing *> appExpr)

appExpr :: Parser Expr
appExpr = some (atom <* spacing) & fmap extract
  where
    extract [] = error "Error parsing appExpr"
    extract [e] = e
    extract (e : es) = ApplyExpr e es

-- appExpr :: Parser Expr
-- appExpr = fmap extract atoms
--   where
--     -- atoms = sepBy1 atom spacing
--     atoms :: Parser [Expr]
--     atoms = do
--         a1 <- atom <* spacing
--         a2 <- atom
--         aTail <- many (spacing *> atom)
--         return $ a1 : a2 : aTail

--     extract [] = error "Error parsing appExpr"
--     extract [e] = e
--     extract (e : es) = ApplyExpr e es

atom :: Parser Expr
atom = littExpr <|> nameExpr <|> parensed expr
  where
    littExpr = fmap LitExpr literal
    nameExpr = fmap IdentifierExpr identifier

associateBinaryOpsL :: Parser (a -> a -> a) -> Parser a -> Parser a
associateBinaryOpsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = Data.List.foldl' (\acc (combine, a) -> combine acc a)

associateBinaryOpsR :: Parser (a -> a -> a) -> Parser a -> Parser a
associateBinaryOpsR sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    shift (oldStart, stack) (combine, a) =
      (a, (combine, oldStart) : stack)

    squash start annotated =
        let (start', annotated') = Data.List.foldl' shift (start, []) annotated
          in Data.List.foldl' (\acc (combine, a) -> combine a acc) start' annotated'


binExpr :: Parser Expr
binExpr = dollarExpr
  where
    dollarExpr = associateBinaryOpsR (BinExpr Dollar <$ (char '$' <* spacing)) orExpr

orExpr :: Parser Expr
orExpr = associateBinaryOpsR (BinExpr Or <$ parseThenSpace "||") andExpr

andExpr :: Parser Expr
andExpr = associateBinaryOpsR (BinExpr And <$ parseThenSpace "&&") comparisonExpr

comparisonExpr :: Parser Expr
comparisonExpr = associateBinaryOpsL comparisonOperator addSubExpr
  where
    comparisonOperator =
      (BinExpr Less <$ parseThenSpace "<") <|>
      (BinExpr LessEqual <$ parseThenSpace "<=") <|>
      (BinExpr Greater <$ parseThenSpace ">") <|>
      (BinExpr GreaterEqual <$ parseThenSpace ">=") <|>
      (BinExpr EqualTo <$ parseThenSpace "==") <|>
      (BinExpr NotEqualTo <$ parseThenSpace "!=")

addSubExpr :: Parser Expr
addSubExpr = associateBinaryOpsL addSubOperator mulDivExpr
  where
    addSubOperator = (BinExpr Add <$ parseThenSpace "+") <|> (BinExpr Sub <$ parseThenSpace "-")

mulDivExpr :: Parser Expr
mulDivExpr = associateBinaryOpsL mulDivOperator unaryExpr
  where
    mulDivOperator = (BinExpr Mul <$ parseThenSpace "*") <|> (BinExpr Div <$ parseThenSpace "/")


-- letExpr :: Parser Expr
-- letExpr =
--   do
--     stringParse "let"
--     spacing1
--     def <- valueDefinition
--     -- defs <- sepBy1 valueDefinition indentAfterEndOfLine
--     spacing1
--     stringParse "in"
--     spacing1
--     LetExpr (Prelude.map ValueDefinition [def]) <$> expr
    -- liftA2 LetExpr ( (stringParse "let" <* spacing1) *> braced valueDefinition) (token In *> expr)


ifExpr :: Parser Expr
ifExpr =
  do
    stringParse "if"
    anySpacing
    e1 <- expr
    anySpacing
    stringParse "then"
    spacing1
    e2 <- expr
    anySpacing
    stringParse "else"
    spacing1
    IfExpr e1 e2 <$> expr
-- ifExpr = IfExpr
--         <$> ( (stringParse "if" <* space) *> expr)
--         <*> (stringParse "then" *> expr)
--         <*> (stringParse "else" *> expr)


lambdaExpr :: Parser Expr
lambdaExpr = char '\\' *> spacing *> liftA2 LambdaExpr (some (identifier <* spacing)) (stringParse "->" *> spacing *> expr)

unspacedPattern :: Parser Pattern
unspacedPattern = simplePattern <|> parensed pattern
  where
    simplePattern =
        singleConstructor
        <|> wildCardPattern
        <|> varPattern
        <|> litPattern
    singleConstructor = fmap (`ConstructorPattern` []) identifier
    wildCardPattern = WildcardPattern <$ char '_'
    varPattern = fmap NamePattern identifier
    litPattern = fmap LiteralPattern literal

pattern :: Parser Pattern
pattern = unspacedPattern <|> argfulPattern
  where
    argfulPattern = liftA2 ConstructorPattern typeIdentifier (some unspacedPattern)

sepBy1 :: Parser a -> Parser esp -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

braced :: Parser a -> Parser [a]
braced p =
    (char '{' <|> char '\n') *> sepBy1 p endOfLine <* (char '}' <|> char '\n')

caseExpr =
    liftA2 CaseExpr (stringParse "case" *> expr <* stringParse "of") (braced patternDef)
    where
      patternDef = liftA2 (,) pattern (stringParse "->" *> expr)

expr :: Parser Expr
-- expr = ifExpr <|> lambdaExpr <|> binExpr <|> caseExpr
expr = ifExpr <|> lambdaExpr <|> binExpr
-- expr = ifExpr <|> binExpr

singleType :: Parser Type
singleType = fmap TVar lowercaseIdentifier <|> primType <|> parensed typeExpr
  where
    primType :: Parser Type
    primType =
      (IntT <$ stringParse "Number")
      <|> (StringT <$ stringParse "String")
      <|> (BoolT <$ stringParse "Boolean")

typeExpr :: Parser Type
-- typeExpr = associateBinaryOpsR ((:->) <$ (spacing *> stringParse "->" <* spacing)) baseType
typeExpr = associateBinaryOpsR ((:->) <$ (basicSpaces *> stringParse "->" <* basicSpaces)) baseType
  where
    baseType :: Parser Type
    baseType = singleType <|> customType

    customType = liftA2 CustomType typeIdentifier (many typeArgument)

-- typeExpr :: Parser Type
-- typeExpr = fmap (\ta -> Data.List.foldl' (:->) (Prelude.head ta) (Prelude.tail ta)) typeArray
--   where
--     typeArray = sepBy1 baseType (basicSpaces *> stringParse "->" <* basicSpaces)

--     baseType :: Parser Type
--     baseType = singleType <|> customType

--     customType :: Parser Type
--     customType = liftA2 CustomType typeIdentifier (many typeArgument)

-- associateBinaryOpR sep p = liftA2 squash p (many (liftA2 (,) sep p))
--   where
--     shift (oldStart, stack) (combine, a) =
--       (a, (combine, oldStart) : stack)

--     squash start annotated =
--         let (start', annotated') = Data.List.foldl' shift (start, []) annotated
--           in Data.List.foldl' (\acc (combine, a) -> combine a acc) start' annotated'

typeArgument :: Parser Type
typeArgument = namedType <|> singleType
  where
    namedType = fmap (`CustomType` []) identifier

typeAnnotation :: Parser ValueDefinition
typeAnnotation = liftA2 TypeAnnotation lowercaseIdentifier ((spacing *> stringParse "::" <* spacing) *> typeExpr)

-- valueDefinition :: Parser ValueDefinition
-- valueDefinition = nameDefinition <|> typeAnnotation
--   where
--     nameDefinition =
--       NameDefinition
--         <$> (lowercaseIdentifier <* space)
--         <*> many unspacedPattern
--         <*> ( (space *> char '=' <* space) *> expr)
    -- typeAnnotation =
    --   liftA2 TypeAnnotation identifier (stringParse "::" *> typeExpr)


valueDefinition :: Parser ValueDefinition
valueDefinition = do
    bindIdentifier <- lowercaseIdentifier <* spacing
    tAnnotation bindIdentifier <|> nDefinition bindIdentifier
  where
    -- ta :: Parser ValueDefinition
    -- ta bindIdentifier = liftA2 TypeAnnotation bindIdentifier ((space *> stringParse "::" <* space) *> typeExpr)
    tAnnotation :: Text -> Parser ValueDefinition
    tAnnotation bindIdentifier = do
        spacing *> stringParse "::" <* spacing
        TypeAnnotation bindIdentifier <$> typeExpr
    nDefinition :: Text -> Parser ValueDefinition
    nDefinition bindIdentifier = do
        patterns <- many unspacedPattern
        body <- (spacing *> char '=' <* spacing) *> expr
        return $ NameDefinition bindIdentifier patterns body

constructorDefinition :: Parser ConstructorDefinition
constructorDefinition = liftA2 ConstructorDefinition identifier (many (typeArgument <* spacing))
-- constructorDefinition = liftA2 ConstructorDefinition identifier (sepBy1 typeArgument spacing1)

definition :: Parser Definition
definition = valueDefinition' <|> dataDefinition <|> typeSynonym
  where
    valueDefinition' = fmap ValueDefinition valueDefinition
    dataDefinition =
        DataDefinition
            <$> ((stringParse "data" <* spacing1) *> identifier <* spacing1)
            <*> many (lowercaseIdentifier <* spacing1)
            <*> (char '=' *> spacing1 *> sepBy1 constructorDefinition (char '|' <* spacing1))
    typeSynonym = liftA2 TypeSynonym
        (stringParse "type" *> identifier) (char '=' *> typeExpr)

definitions :: Parser [Definition]
definitions = sepBy1 definition endOfLine

ast :: Parser AST
ast = fmap (\d -> AST {
    functions = d
    }) definitions
-- ast = fmap AST definitions

-- spaceOrComment :: Parser ()
-- spaceOrComment = L.space space1 (L.skipLineComment $ pack "--") (L.skipBlockComment (pack "{-") (pack "-}"))


