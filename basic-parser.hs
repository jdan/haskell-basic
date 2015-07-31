import Control.Monad
import Text.ParserCombinators.Parsec

{-
data Line = NumberedLine BasicNumber Statement | Statement
type Program = [Line]
-}

data Var = Var Char deriving (Show)

data BasicNumber = Number Integer deriving (Show)
data BasicString = String String deriving (Show)

data Expression = BareExpression Term
                | PlusExpression Term Term
                | MinusExpression Term Term
                | UnaryPlusExpression Term
                | UnaryMinusExpression Term
                deriving (Show)

data Term = BareTerm Factor
          | MultiplyTerm Factor Factor
          | DivideTerm Factor Factor
          deriving (Show)

data Factor = VarFactor Var
            | BasicNumberFactor BasicNumber
            | ExpressionFactor Expression
            deriving (Show)

data Statement = PrintStatement Expression deriving (Show)

-- EXPRESSIONS
-- expression ::= (+|-|ε) term ((+|-) term)*
parseBareExpression :: Parser Expression
parseBareExpression = do
    term <- parseTerm
    return $ BareExpression term

parseBinaryExpression :: Char -> (Term -> Term -> Expression) -> Parser Expression
parseBinaryExpression op typeclass = do
    left <- parseTerm
    spaces
    char op
    spaces
    right <- parseTerm
    return $ typeclass left right

parsePlusExpression = parseBinaryExpression '+' PlusExpression
parseMinusExpression = parseBinaryExpression '-' MinusExpression

parseUnaryExpression :: Char -> (Term -> Expression) -> Parser Expression
parseUnaryExpression op typeclass = do
    char op
    spaces
    term <- parseTerm
    return $ typeclass term

parseUnaryPlusExpression = parseUnaryExpression '+' UnaryPlusExpression
parseUnaryMinusExpression = parseUnaryExpression '-' UnaryMinusExpression

parseExpression :: Parser Expression
parseExpression =
    try parsePlusExpression <|>
    try parseMinusExpression <|>
    try parseUnaryPlusExpression <|>
    try parseUnaryMinusExpression <|>
    parseBareExpression

-- FACTORS
-- factor ::= var | number | (expression)
parseVarFactor = VarFactor <$> parseVar
parseBasicNumberFactor = BasicNumberFactor <$> parseNumber

-- Parse an expression factor, or an expression surrounded by parentheses
parseExpressionFactor = do
    char '('
    expression <- parseExpression
    char ')'
    return $ ExpressionFactor expression

parseFactor :: Parser Factor
parseFactor = parseVarFactor <|> parseBasicNumberFactor <|> parseExpressionFactor

-- TERMS
-- term ::= factor ((*|/) factor)*
parseBareTerm :: Parser Term
parseBareTerm = BareTerm <$> parseFactor

parseBinaryTerm :: Char -> (Factor -> Factor -> Term) -> Parser Term
parseBinaryTerm op typeclass = do
    left <- parseFactor
    spaces
    char op
    spaces
    right <- parseFactor
    return $ typeclass left right

parseMultiplyTerm = parseBinaryTerm '*' MultiplyTerm
parseDivideTerm = parseBinaryTerm '/' DivideTerm

parseTerm :: Parser Term
parseTerm = try parseMultiplyTerm <|> try parseDivideTerm <|> parseBareTerm

-- NUMBERS
parseNumber :: Parser BasicNumber
parseNumber = liftM (Number . read) $ many1 digit

-- STRINGS
parseString :: Parser BasicString
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

-- VARS
-- var ::= A | B | C ... | Y | Z
parseVar = Var <$> upper
