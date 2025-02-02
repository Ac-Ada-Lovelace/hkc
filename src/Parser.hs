{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Parser(
    Parser,
    runParser,
    ppair,
    pmap,
    left,
    right,
    identifier,
    getNumeric,
    tokenize

)where

import Source(Source, readChar, closeSource)
import GHC.Base (Alternative (..))
import Prelude hiding (map)
import GHC.Unicode (isAlpha, isAlphaNum)
import AST (Token, TokenType(..), OperatorType(..), Token(..),
                keywords,operators, keyWordTypeFromString)

-- 定义解析器类型
newtype Parser a = Parser {runParser :: String -> Either String (String, a)}


instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Left _ -> p2 input
    Right res -> Right res
-- 实现 Functor, Applicative, Monad 实例
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Left err -> Left err
    Right (rest, result) -> Right (rest, f result)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser pf) <*> (Parser p) = Parser $ \input -> case pf input of
    Left err -> Left err
    Right (rest, f) -> case p rest of
      Left err -> Left err
      Right (rest', result) -> Right (rest', f result)

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> case p input of
    Left err -> Left err
    Right (rest, result) -> runParser (f result) rest

ppair :: Parser a -> Parser b -> Parser (a, b)
ppair (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Left err -> Left err
    Right (rest1, result1) -> case p2 rest1 of
        Left err -> Left err
        Right (rest2, result2) -> Right (rest2, (result1, result2))


pmap :: Parser a -> (a -> b) -> Parser b
pmap (Parser p) f = Parser $ \input -> case p input of
    Left err -> Left err
    Right (rest, result) -> Right (rest, f result)
left :: Parser a -> Parser b -> Parser a
left (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Left err -> Left err
    Right (rest1, result1) -> case p2 rest1 of
        Left err -> Left err
        Right (rest2, _) -> Right (rest2, result1)

right :: Parser a -> Parser b -> Parser b
right (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Left err -> Left err
    Right (rest1, _) -> p2 rest1

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore  = some

predicate ::Parser a -> (a -> Bool) -> Parser a
predicate (Parser p) f = Parser $ \input -> case p input of
    Left err -> Left err
    Right (rest, result) -> if f result
        then Right (rest, result)
        else Left "predicate failed"

anyChar :: Parser Char
anyChar = Parser $ \input -> case input of
    [] -> Left "empty"
    (x:xs) -> Right (xs, x)

peekChar :: Parser Char
peekChar = Parser $ \input -> case input of
    [] -> Left "empty"
    (x:_) -> Right (input, x)

whiteSpace :: Parser Char
whiteSpace = predicate anyChar (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

space0 :: Parser [Char]
space0 = zeroOrMore whiteSpace

space1 :: Parser [Char]
space1 = oneOrMore whiteSpace


-- 定义 identifier 函数
identifier :: Parser String
identifier = Parser $ \input -> case input of
  [] -> Left "Expected identifier"
  (c : cs) ->
    if isAlpha c || c == '_'
      then
        let (name, rest) = span (\x -> isAlphaNum x || x == '_') cs
         in Right (rest, c : name)
      else Left "Expected identifier"

numeric :: Parser String
numeric = Parser $ \input -> case input of
    [] -> Left "empty"
    (x:xs) -> if x `elem` ['0'..'9']
        then Right (xs, [x])
        else Left "not a numeric"

getNumeric :: Parser String
getNumeric = do
    _ <- space0
    oneOrMore (predicate anyChar (`elem` ['0'..'9']))



tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isOperatorStarts (c:take 1 cs) = tokenizeOperator (c:take 1 cs) (drop 1 cs)
    | isOperatorStarts [c] = tokenizeOperator [c] cs
    | Parser.isDigit c = tokenizeNumber (c:cs)
    | isAlpha c || c == '_' = tokenizeIdentifier (c:cs)
    | otherwise = error "unexpected character"

isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r']

isOperatorStarts :: String -> Bool
isOperatorStarts op = op `elem` operators

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

tokenizeOperator :: String -> String -> [Token]
tokenizeOperator op cs = Token (operatorType op) op : tokenize cs
  where
    operatorType "+" = T_Operator T_Add
    operatorType "-" = T_Operator T_Sub
    operatorType "*" = T_Operator T_Multiply
    operatorType "/" = T_Operator T_Divide
    operatorType "%" = T_Operator T_Mod
    operatorType "=" = T_Operator T_Assign
    operatorType "+=" = T_Operator T_AddAssign
    operatorType "-=" = T_Operator T_SubAssign
    operatorType "*=" = T_Operator T_MulAssign
    operatorType "/=" = T_Operator T_DivAssign
    operatorType "==" = T_Operator T_Equal
    operatorType "!=" = T_Operator T_NotEqual
    operatorType "<" = T_Operator T_Less
    operatorType ">" = T_Operator T_Greater
    operatorType "<=" = T_Operator T_LessEqual
    operatorType ">=" = T_Operator T_GreaterEqual
    operatorType "&&" = T_Operator T_LogicalAnd
    operatorType "||" = T_Operator T_LogicalOr
    operatorType "&" = T_Operator T_BitAnd
    operatorType "|" = T_Operator T_BitOr
    operatorType "^" = T_Operator T_BitXor
    operatorType "~" = T_Operator T_BitNot
    operatorType "<<" = T_Operator T_LeftShift
    operatorType ">>" = T_Operator T_RightShift
    operatorType "!" = T_Operator T_LogicalNot
    operatorType _ = error "unexpected operator"

tokenizeNumber :: String -> [Token]
tokenizeNumber str = case runParser getNumeric str of
    Right (rest, num) -> Token T_TNumber num : tokenize rest
    Left err -> error $ "unexpected character: " ++ err

tokenizeIdentifier :: String -> [Token]
tokenizeIdentifier str = case runParser identifier str of
    Right (rest, iden) -> case iden of
        _ | iden `elem` keywords -> Token (T_Keyword $ keyWordTypeFromString iden) iden : tokenize rest
          | otherwise -> Token T_Identifier iden : tokenize rest
    Left err -> error $ "unexpected character: " ++ err

startParsing :: Source -> IO ()
startParsing source = do
    content <- readAllChars source
    let res = runParser (ppair identifier numeric) content
    print res
    closeSource source

readAllChars :: Source -> IO String
readAllChars source = do
    maybeChar <- readChar source
    case maybeChar of
        Nothing -> return []
        Just char -> do
            rest <- readAllChars source
            return (char : rest)