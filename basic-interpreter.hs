module Main where

import Control.Monad.Trans
import Control.Applicative hiding ((<|>), many)
import Data.List

import Text.ParserCombinators.Parsec

#ifdef __GHCJS__
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.Node
import GHCJS.Types
#endif

-- "Escape" string to clear the screen (read by UI code)
clearEscape = "[[BB"

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
evalVar (Var v) env = getVariable env v


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
               | GoSubStatement Expression
               | ClearStatement
               | ReturnStatement
               deriving (Show)

parseStatement :: Parser Statement
parseStatement =
    parsePrintStatement <|>
    parseLetStatement <|>
    parseIfStatement <|>
    try parseGotoStatement <|>
    parseGoSubStatement <|>
    parseReturnStatement <|>
    parseClearStatement

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

        parseGoSubStatement :: Parser Statement
        parseGoSubStatement = do
            string "GOSUB"
            spaces
            expression <- parseExpression
            return $ GoSubStatement expression

        parseClearStatement :: Parser Statement
        parseClearStatement = do
            string "CLEAR"
            return ClearStatement

        parseReturnStatement :: Parser Statement
        parseReturnStatement = do
            string "RETURN"
            return ReturnStatement

labelToLine :: Environment -> Int -> Int
labelToLine env label = newLine
    where
        newLine = case (findIndex labelMatches $ getProgram env) of
            Nothing -> -1
            Just index -> index

        labelMatches :: BasicLine -> Bool
        labelMatches line = case line of
            UnnumberedLine _ -> False
            EmptyLine -> False
            NumberedLine (Number num) _ -> (num == label)

evalStatement :: Statement -> Environment -> Environment

evalStatement (PrintStatement expression) env =
    setMessage env ((Just . show) $ evalExpression expression env)

evalStatement (LetStatement (Var v) expression) env =
    -- Append a frame to the environment
    setVariable env v $ evalExpression expression env

evalStatement (IfStatement left relop right statement) env =
    if comparison then evalStatement statement env
                  else env
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
    setCurrentLine env newLine
    where
        newLine = labelToLine env label
        label = evalExpression expression env

evalStatement (GoSubStatement expression) env =
    setCurrentLine newEnv newLine
    where
        -- Push the next line to the stack
        -- NOTE: We increment the current line prematurely before evaluating
        -- this statment
        newEnv = pushStack env $ getCurrentLine env

        -- Fetch the label we will be jumping to
        label = evalExpression expression newEnv

        -- Convert the label to a tangible line number
        newLine = labelToLine newEnv label


evalStatement (ClearStatement) env = setMessage env $ Just clearEscape

evalStatement (ReturnStatement) env =
    setCurrentLine newEnv line
    where
        (line, newEnv) = popStack env

-- LINES
-- line ::= number statement CR | statement CR
data BasicLine = NumberedLine BasicNumber Statement
               | UnnumberedLine Statement
               | EmptyLine
               deriving (Show)

parseLine :: Parser BasicLine
parseLine = parseNumberedLine <|> parseUnnumberedLine <|> parseEmptyLine
    where
        parseNumberedLine :: Parser BasicLine
        parseNumberedLine = do
            number <- parseNumber
            spaces
            statement <- parseStatement
            return $ NumberedLine number statement

        parseUnnumberedLine :: Parser BasicLine
        parseUnnumberedLine = UnnumberedLine <$> parseStatement

        parseEmptyLine :: Parser BasicLine
        parseEmptyLine = do
            spaces
            return EmptyLine

evalLine :: BasicLine -> Environment -> Environment
evalLine (NumberedLine _ statement) env = evalStatement statement env
evalLine (UnnumberedLine statement) env = evalStatement statement env
evalLine EmptyLine env = env


-- ENVIRONMENT
type Environment = (
    -- The program code
    [BasicLine],

    -- The list of variable declarations
    [(Char, Int)],

    -- The stack
    [Int],

    -- The current line
    Int,

    -- A message to print
    Maybe String)

initialEnvironment :: [BasicLine] -> Environment
initialEnvironment program = (program, [], [(-1)], 0, Nothing)

getVariable :: Environment -> Char -> Int
getVariable (program, store, stack, line, message) key = case (lookup key store) of
    Just value -> value
    -- Variables have a default value of 0
    Nothing -> 0

setVariable :: Environment -> Char -> Int -> Environment
setVariable (program, store, stack, line, message) key value =
    (program, [(key, value)] ++ store, stack, line, message)

pushStack :: Environment -> Int -> Environment
pushStack (program, store, stack, line, message) item =
    (program, store, item:stack, line, message)

popStack :: Environment -> (Int, Environment)
popStack (_, _, [], _, _) = error "Popping empty stack"
popStack (program, store, first:rest, line, message) =
    (first, (program, store, rest, line, message))

getCurrentLine :: Environment -> Int
getCurrentLine (_, _, _, line, _) = line

setCurrentLine :: Environment -> Int -> Environment
setCurrentLine (program, store, stack, line, message) newLine =
    (program, store, stack, newLine, message)

getMessage :: Environment -> Maybe String
getMessage (_, _, _, _, message) = message

setMessage :: Environment -> Maybe String -> Environment
setMessage (program, store, stack, line, message) newMessage =
    (program, store, stack, line, newMessage)

getProgram :: Environment -> [BasicLine]
getProgram (program, _, _, _, _) = program


-- Run an entire program
parseProgram :: [String] -> Either ParseError [BasicLine]
parseProgram program = helper program []
    where
        helper [] acc = Right acc
        helper (line:rest) acc = case (parse parseLine "" line) of
            Right line -> helper rest (acc ++ [line])
            Left err -> Left err

evalProgram :: Environment -> (String -> IO()) -> IO(Environment)
evalProgram env printer = do
    -- Shall we print something?
    case (getMessage env) of
        Nothing -> return ()
        Just message -> printer message

    if (getCurrentLine env) < 0
        then return env
        else
            return evalProgram program newEnv printer
                where
                    newEnv = evalLine (program !! line) nextEnv

                    -- Clear out the current message and increase the line by 1
                    nextEnv = setMessage (setCurrentLine env $ (+1) line) Nothing
                    line = getCurrentLine env
                    program = getProgram env


#ifdef __GHCJS__
main = do
    -- Running a GUI creates a WebKitGtk window in native code,
    -- but just returns the browser window when compiled to JavaScript
    runWebGUI $ \ webView -> do
        Just doc <- webViewGetDomDocument webView
        Just body <- documentGetBody doc

        Just code <- fmap castToHTMLTextAreaElement <$> documentGetElementById doc "basic"
        Just output <- fmap castToHTMLElement <$> documentGetElementById doc "output"
        Just run <- fmap castToHTMLElement <$> documentGetElementById doc "run"

        let
            -- Append a message to the output <ul>
            appendContent message = do
                if message == clearEscape
                    then do
                        htmlElementSetInnerHTML output (toJSString "")
                    else do
                        Just entry <- fmap castToHTMLElement <$> documentCreateElement doc "li"
                        htmlElementSetInnerHTML entry (toJSString message)
                        nodeAppendChild output (Just entry)
                        return ()

            runCode = do
                htmlElementSetInnerHTML output (toJSString "")
                inputCode <- htmlTextAreaElementGetValue code

                case (parseProgram (lines $ fromJSString inputCode)) of
                    Left err -> do
                        htmlElementSetInnerHTML output (show err)
                        return ()
                    Right program -> do
                        evalProgram (initialEnvironment program) appendContent
                        return ()

        elementOnclick run (liftIO runCode)
        return ()
#else
main = do
    input <- getContents
    eitherAst <- (return . parseProgram . lines) input
    case eitherAst of
        Right ast -> do
            evalProgram (initialEnvironment ast) putStrLn
            return ()
        Left err -> print err
#endif
