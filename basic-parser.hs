import Control.Monad
import Text.ParserCombinators.Parsec

-- LINES
-- line ::= number statement CR | statement CR
data BasicLine = NumberedLine BasicNumber Statement
               | UnnumberedLine Statement
               deriving (Show)

parseLine :: Parser BasicLine
parseLine = parseNumberedLine <|> parseUnnumberedLine
    where
        parseNumberedLine :: Parser BasicLine
        parseNumberedLine = do
            number <- parseNumber
            spaces
            statement <- parseStatement
            return $ NumberedLine number statement

        parseUnnumberedLine :: Parser BasicLine
        parseUnnumberedLine = UnnumberedLine <$> parseStatement

-- EXPRESSIONS
-- expression ::= (+|-|ε) term ((+|-) term)*
data Expression = BareExpression Term
                | PlusExpression Term Term
                | MinusExpression Term Term
                | UnaryPlusExpression Term
                | UnaryMinusExpression Term
                deriving (Show)

parseExpression :: Parser Expression
parseExpression =
    try parsePlusExpression <|>
    try parseMinusExpression <|>
    try parseUnaryPlusExpression <|>
    try parseUnaryMinusExpression <|>
    parseBareExpression

    where
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

-- FACTORS
-- factor ::= var | number | (expression)
data Factor = VarFactor Var
            | BasicNumberFactor BasicNumber
            | ExpressionFactor Expression
            deriving (Show)

parseFactor :: Parser Factor
parseFactor =
    parseVarFactor <|> parseBasicNumberFactor <|> parseExpressionFactor

    where
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

-- TERMS
-- term ::= factor ((*|/) factor)*
data Term = BareTerm Factor
          | MultiplyTerm Factor Factor
          | DivideTerm Factor Factor
          deriving (Show)

parseTerm :: Parser Term
parseTerm = try parseMultiplyTerm <|> try parseDivideTerm <|> parseBareTerm
    where
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

-- STATEMENTS
data Statement = PrintStatement Expression
               | LetStatement Var Expression
               deriving (Show)

parseStatement :: Parser Statement
parseStatement = parsePrintStatement <|> parseLetStatement
    where
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

-- NUMBERS
data BasicNumber = Number Integer deriving (Show)

parseNumber :: Parser BasicNumber
parseNumber = Number . read <$> many1 digit

-- STRINGS
data BasicString = String String deriving (Show)

parseString :: Parser BasicString
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

-- VARS
-- var ::= A | B | C ... | Y | Z
data Var = Var Char deriving (Show)

parseVar :: Parser Var
parseVar = Var <$> upper
