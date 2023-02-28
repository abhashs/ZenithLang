{-# LANGUAGE GADTs #-}

module Parser where
import Data.Void
import Data.Text
import Text.Megaparsec (Parsec, (<|>), runParser, parse, errorBundlePretty, many, noneOf, oneOf, try, some)
import Text.Megaparsec.Char (string, char, digitChar, letterChar, alphaNumChar, space, tab, space1)

type MParser = Parsec Void Text

data LExpr
    = LInt Int
    | LBool Bool
    | LString String
    | LVar String
    | LApp LExpr LExpr
    | LError String
    deriving (Show, Eq)

data LStatement = LStatement LExpr [LExpr] LExpr
    deriving (Show, Eq)

boolean :: MParser LExpr
boolean =
    (trueParser >> return (LBool True)) <|>
    (falseParser >> return (LBool False))
      where
        trueParser = string $ pack "True"
        falseParser = string $ pack "False"

number:: MParser LExpr
number= do
    x <- many digitChar
    return $ LInt $ read x

escapedChars :: MParser Char
escapedChars = do
    char '\\'
    oneOf "\\\""

stringParser :: MParser LExpr
stringParser = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ LString x

identifier :: MParser LExpr
identifier = do
    x <- letterChar <|> char '_'
    xs <- many $ alphaNumChar <|> char '_'
    return $ LVar (x:xs)

atom :: MParser LExpr
atom = number <|> boolean <|> stringParser

application:: MParser LExpr
application = do
    e1 <- expr
    LApp e1 <$> expr

singleExpr :: MParser LExpr
singleExpr = atom <|> identifier

-- expr :: MParser LExpr
-- expr = some $ space1 >> singleExpr

expr :: MParser LExpr
expr = atom <|> identifier <|> application

implParser :: MParser LStatement
implParser = do
    var <- identifier
    args <- many $ try $ do
        space1
        identifier
    space
    char '='
    space
    LStatement var args <$> expr

-- Top level parser for entire program
programParser :: MParser [LStatement]
programParser = many $ do
    s <- implParser
    char '\n'
    return s

parseString = parse expr ""

readExpr :: Text -> LExpr
readExpr expr = case parseString expr of
    Left e -> LError $ errorBundlePretty e
    Right v -> v


