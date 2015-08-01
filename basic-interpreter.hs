module Main where

import Control.Applicative hiding ((<|>), many)

import Text.ParserCombinators.Parsec

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types

-- ENVIRONMENT
type Environment = [(Char, Int)]

setEnvironment :: Environment -> Char -> Int -> Environment
setEnvironment env key value = [(key, value)] ++ env

getEnvironment :: Environment -> Char -> Int
getEnvironment env key = case (lookup key env) of
    Just value -> value
    -- Variables have a default value of 0
    Nothing -> 0


-- NUMBERS
data BasicNumber = Number Int deriving (Show)

parseNumber :: Parser BasicNumber
parseNumber = Number . read <$> many1 digit

evalNumber :: BasicNumber -> Environment -> Int
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

evalVar :: Var -> Environment -> Int
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

evalFactor :: Factor -> Environment -> Int
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

evalTerm :: Term -> Environment -> Int
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

evalExpression :: Expression -> Environment -> Int
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
               | IfStatement Expression String Expression Statement
               | GotoStatement Expression
               deriving (Show)

parseStatement :: Parser Statement
parseStatement =
    parsePrintStatement <|>
    parseLetStatement <|>
    parseIfStatement <|>
    parseGotoStatement
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

        parseRelOp :: Parser String
        parseRelOp =
            (try $ string "<=") <|>
            (try $ string ">=") <|>
            (try $ string "<>") <|>
            string "<"  <|>
            string ">"  <|>
            string "="

        parseIfStatement :: Parser Statement
        parseIfStatement = do
            string "IF"
            spaces
            left <- parseExpression
            spaces
            relop <- parseRelOp
            spaces
            right <- parseExpression
            spaces
            string "THEN"
            spaces
            statement <- parseStatement
            return $ IfStatement left relop right statement

        parseGotoStatement :: Parser Statement
        parseGotoStatement = do
            string "GOTO"
            spaces
            expression <- parseExpression
            return $ GotoStatement expression

-- Statements return:
--   a new environment
--   maybe a string to output to the console
--   maybe a line to jump to
evalStatement :: Statement -> Environment -> (Environment, Maybe String, Maybe Int)

evalStatement (PrintStatement expression) env =
    -- No change to the environment, but send through a string that we will
    -- print out
    (env, (Just . show) $ evalExpression expression env, Nothing)
evalStatement (LetStatement (Var v) expression) env =
    -- Append a frame to the environment
    (setEnvironment env v $ evalExpression expression env, Nothing, Nothing)

evalStatement (IfStatement left relop right statement) env =
    if comparison then evalStatement statement env
                  else (env, Nothing, Nothing)
    where
        leftResult = evalExpression left env
        rightResult = evalExpression right env

        comparison = case relop of
            "<"  -> leftResult < rightResult
            ">"  -> leftResult > rightResult
            "="  -> leftResult == rightResult
            "<=" -> leftResult <= rightResult
            ">=" -> leftResult >= rightResult
            "<>" -> leftResult /= rightResult

evalStatement (GotoStatement expression) env =
    (env, Nothing, Just $ evalExpression expression env)


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

evalLine :: BasicLine -> Environment -> (Environment, Maybe String, Maybe Int)
evalLine (NumberedLine _ statement) env = evalStatement statement env
evalLine (UnnumberedLine statement) env = evalStatement statement env


-- Run an entire program
parseProgram :: [String] -> Either ParseError [BasicLine]
parseProgram program = helper program []
    where
        helper [] acc = Right acc
        helper (line:rest) acc = case (parse parseLine "" line) of
            Right line -> helper rest (acc ++ [line])
            Left err -> Left err


evalProgram :: [BasicLine] -> Environment -> Int -> (String -> IO()) -> IO(Environment)
evalProgram program env line printer
    | line > (length program) = return env
    | otherwise = do
        let (nextEnv, message, nextLine) = evalLine (program !! line) env in
            do
                case message of
                    Nothing -> return ()
                    Just message -> printer message
                case nextLine of
                    Nothing -> evalProgram program nextEnv (line + 1) printer
                    Just lineNo -> evalProgram program nextEnv targetLine printer
                        where
                            targetLine = 0


{-
main = do
    input <- getContents
    case (parseProgram (lines input)) of
        Left err -> print err
        Right program -> do
            evalProgram program [] 1 putStrLn
            return ()
-}

main = do
    runWebGUI $ \ webView -> do
        Just doc <- webViewGetDomDocument webView
        Just output <- fmap castToHTMLDivElement <$> documentGetElementById doc "output"
        htmlElementSetInnerText output "Hello"
