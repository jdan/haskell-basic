import Control.Monad
import Text.ParserCombinators.Parsec


-- ENVIRONMENT
type Environment = [(Char, Integer)]

setEnvironment :: Environment -> Char -> Integer -> Environment
setEnvironment env key value = [(key, value)] ++ env

getEnvironment :: Environment -> Char -> Integer
getEnvironment env key = case (lookup key env) of
    Just value -> value
    -- Variables have a default value of 0
    Nothing -> 0


-- CONSOLE
type Console = [String]


-- NUMBERS
data BasicNumber = Number Integer deriving (Show)

parseNumber :: Parser BasicNumber
parseNumber = Number . read <$> many1 digit

evalNumber :: BasicNumber -> Environment -> Integer
evalNumber (Number num) _ = num


-- STRINGS
data BasicString = String String deriving (Show)

parseString :: Parser BasicString
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

evalString :: BasicString -> Environment -> String
evalString (String str) _ = str


-- VARS
-- var ::= A | B | C ... | Y | Z
data Var = Var Char deriving (Show)

parseVar :: Parser Var
parseVar = Var <$> upper

evalVar :: Var -> Environment -> Integer
evalVar (Var v) env = getEnvironment env v


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

evalFactor :: Factor -> Environment -> Integer
evalFactor (VarFactor v) env = evalVar v env
evalFactor (BasicNumberFactor num) env = evalNumber num env
evalFactor (ExpressionFactor exp) env = evalExpression exp env


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

evalTerm :: Term -> Environment -> Integer
evalTerm (BareTerm factor) env = evalFactor factor env
evalTerm (MultiplyTerm left right) env = leftResult * rightResult
    where
        leftResult = evalFactor left env
        rightResult = evalFactor right env
evalTerm (DivideTerm left right) env = leftResult `div` rightResult
    where
        leftResult = evalFactor left env
        rightResult = evalFactor right env


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

evalExpression :: Expression -> Environment -> Integer
evalExpression (BareExpression term) env = evalTerm term env
evalExpression (PlusExpression left right) env = leftResult + rightResult
    where
        leftResult = evalTerm left env
        rightResult = evalTerm right env
evalExpression (MinusExpression left right) env = leftResult - rightResult
    where
        leftResult = evalTerm left env
        rightResult = evalTerm right env
evalExpression (UnaryPlusExpression term) env = evalTerm term env
evalExpression (UnaryMinusExpression term) env = (-) 0 $ evalTerm term env


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

evalStatement :: Statement -> Environment -> Console -> (Environment, Console)
-- Print to the console
evalStatement (PrintStatement expression) env console = (
    env,
    (show $ evalExpression expression env) : console)
-- Append a frame to the environment
evalStatement (LetStatement (Var v) expression) env console = (
    setEnvironment env v $ evalExpression expression env,
    console)


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

evalLine :: BasicLine -> Environment -> Console -> (Environment, Console)
evalLine (NumberedLine _ statement) env console =
    evalStatement statement env console
evalLine (UnnumberedLine statement) env console =
    evalStatement statement env console
