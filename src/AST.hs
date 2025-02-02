{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AST(
    TokenType(..),
    OperatorType(..),
    KeywordType(..),
    operatorPrecedence,
    Token(..)
)where

data TokenType =
    TNumber
    | Identifier
    | Operator OperatorType
    | Keyword KeywordType
    | SemiColon

instance Show TokenType where
    show TNumber = "Number"
    show Identifier = "Identifier"
    show (Operator op) = show op
    show (Keyword kw) = show kw
    show SemiColon = "SemiColon"

data OperatorType =
    Plus
    | Minus
    | Multiply
    | Divide
    | Mod
    | Assign
    | Equal
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | And
    | Or
    | Not

instance Show OperatorType where
    show Plus = "Plus"
    show Minus = "Minus"
    show Multiply = "Multiply"
    show Divide = "Divide"
    show Mod = "Mod"
    show Assign = "Assign"
    show Equal = "Equal"
    show NotEqual = "NotEqual"
    show Less = "Less"
    show LessEqual = "LessEqual"
    show Greater = "Greater"
    show GreaterEqual = "GreaterEqual"
    show And = "And"
    show Or = "Or"
    show Not = "Not"

data KeywordType =
    If
    | Else
    | While
    | For
    | Break
    | Continue
    | Return
    | Function
    | Var
    | True
    | False

instance Show KeywordType where
    show If = "If"
    show Else = "Else"
    show While = "While"
    show For = "For"
    show Break = "Break"
    show Continue = "Continue"
    show Return = "Return"
    show Function = "Function"
    show Var = "Var"
    show AST.True = "True"
    show AST.False = "False"

operatorPrecedence :: OperatorType -> Int
operatorPrecedence op = case op of
    Or          -> 1
    And         -> 2
    Equal       -> 3
    NotEqual    -> 3
    Less        -> 4
    LessEqual   -> 4
    Greater     -> 4
    GreaterEqual-> 4
    Plus        -> 5
    Minus       -> 5
    Multiply    -> 6
    Divide      -> 6
    Mod         -> 6
    Assign      -> 7
    Not         -> 8

data Token = Token{
    tokenType :: TokenType,
    tokenValue :: String
}
instance Show Token where
    show (Token t v) = "{" ++ show t ++ ": " ++ v ++ "}" 