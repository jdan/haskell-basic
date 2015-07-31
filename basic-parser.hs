import Control.Monad
import Text.ParserCombinators.Parsec

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

data Statement = PrintStatement Expression
               | LetStatement Var Expression
               deriving (Show)

data BasicLine = NumberedLine BasicNumber Statement
               | UnnumberedLine Statement
               deriving (Show)

parseNumberedLine :: Parser BasicLine
parseNumberedLine = do
    number <- parseNumber
    spaces
    statement <- parseStatement
    return $ NumberedLine number statement

parseUnnumberedLine :: Parser BasicLine
parseUnnumberedLine = UnnumberedLine <$> parseStatement

parseLine :: Parser BasicLine
parseLine = parseNumberedLine <|> parseUnnumberedLine

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

parsePlusExpression :: Parser Expression
parsePlusExpression = parseBinaryExpression '+' PlusExpression

parseMinusExpression :: Parser Expression
parseMinusExpression = parseBinaryExpression '-' MinusExpression

parseUnaryExpression :: Char -> (Term -> Expression) -> Parser Expression
parseUnaryExpression op typeclass = do
    char op
    spaces
    term <- parseTerm
    return $ typeclass term

parseUnaryPlusExpression :: Parser Expression
parseUnaryPlusExpression = parseUnaryExpression '+' UnaryPlusExpression

parseUnaryMinusExpression :: Parser Expression
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
parseVarFactor :: Parser Factor
parseVarFactor = VarFactor <$> parseVar

parseBasicNumberFactor :: Parser Factor
parseBasicNumberFactor = BasicNumberFactor <$> parseNumber

-- Parse an expression factor, or an expression surrounded by parentheses
parseExpressionFactor :: Parser Factor
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

-- STATEMENTS
parsePrintStatement :: Parser Statement
parsePrintStatement = do
    string "PRINT"
    spaces
    expression <- parseExpression
    return $ PrintStatement expression

parseLetStatement :: Parser Statement
parseLetStatement = do
    string "LET"
    spaces
    var <- parseVar
    spaces
    char '='
    spaces
    expression <- parseExpression
    return $ LetStatement var expression

parseStatement :: Parser Statement
parseStatement = parsePrintStatement <|> parseLetStatement

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
parseVar :: Parser Var
parseVar = Var <$> upper
