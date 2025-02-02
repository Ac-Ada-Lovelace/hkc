{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module AST(
    TokenType(..),
    OperatorType(..),
    KeywordType(..),
    operatorPrecedence,
    operators,
    Token(..),
    keywords,
    keyWordTypeFromString,
)where

data TokenType =
    T_TNumber
    | T_Identifier
    | T_Operator OperatorType
    | T_Keyword KeywordType
    | T_SemiColon

instance Show TokenType where
    show T_TNumber = "Number"
    show T_Identifier = "Identifier"
    show (T_Operator op) = show op
    show (T_Keyword kw) = show kw
    show T_SemiColon = "SemiColon"

data OperatorType =
    T_SuffixIncrement
    | T_SuffixDecrement
    | T_FunctionCall
    | T_ArrayAccess
    | T_MemberAccess
    | T_MemberAccessThroughPointer
    | T_CompoundLiteral
    | T_PrefixIncrement
    | T_PrefixDecrement
    | T_UnaryPlus
    | T_UnaryMinus
    | T_LogicalNot
    | T_BitNot
    | T_Cast
    | T_Indirection
    | T_AddressOf
    | T_SizeOf
    | T_AlignOf -- _Alignof won't be supported in early versions
    | T_Multiply
    | T_Divide
    | T_Mod
    | T_Add
    | T_Sub
    | T_LeftShift
    | T_RightShift
    | T_Less
    | T_LessEqual
    | T_Greater
    | T_GreaterEqual
    | T_Equal
    | T_NotEqual
    | T_BitAnd
    | T_BitXor
    | T_BitOr
    | T_LogicalAnd
    | T_LogicalOr
    | T_TernaryConditional -- ?: wont be supported in early versions
    | T_Assign
    | T_AddAssign
    | T_SubAssign
    | T_MulAssign
    | T_DivAssign
    | T_ModAssign
    | T_LeftShiftAssign
    | T_RightShiftAssign
    | T_BitAndAssign
    | T_BitXorAssign
    | T_BitOrAssign
    | T_Comma

instance Show OperatorType where
    show T_SuffixIncrement = "SuffixIncrement"
    show T_SuffixDecrement = "SuffixDecrement"
    show T_FunctionCall = "FunctionCall"
    show T_ArrayAccess = "ArrayAccess"
    show T_MemberAccess = "MemberAccess"
    show T_MemberAccessThroughPointer = "MemberAccessThroughPointer"
    show T_CompoundLiteral = "CompoundLiteral"
    show T_PrefixIncrement = "PrefixIncrement"
    show T_PrefixDecrement = "PrefixDecrement"
    show T_UnaryPlus = "UnaryPlus"
    show T_UnaryMinus = "UnaryMinus"
    show T_LogicalNot = "LogicalNot"
    show T_BitNot = "BitNot"
    show T_Cast = "Cast"
    show T_Indirection = "Indirection"
    show T_AddressOf = "AddressOf"
    show T_SizeOf = "SizeOf"
    show T_AlignOf = "AlignOf"
    show T_Multiply = "Multiply"
    show T_Divide = "Divide"
    show T_Mod = "Mod"
    show T_Add = "Add"
    show T_Sub = "Sub"
    show T_LeftShift = "LeftShift"
    show T_RightShift = "RightShift"
    show T_Less = "Less"
    show T_LessEqual = "LessEqual"
    show T_Greater = "Greater"
    show T_GreaterEqual = "GreaterEqual"
    show T_Equal = "Equal"
    show T_NotEqual = "NotEqual"
    show T_BitAnd = "BitAnd"
    show T_BitXor = "BitXor"
    show T_BitOr = "BitOr"
    show T_LogicalAnd = "LogicalAnd"
    show T_LogicalOr = "LogicalOr"
    show T_TernaryConditional = "TernaryConditional"
    show T_Assign = "Assign"
    show T_AddAssign = "AddAssign"
    show T_SubAssign = "SubAssign"
    show T_MulAssign = "MulAssign"
    show T_DivAssign = "DivAssign"
    show T_ModAssign = "ModAssign"
    show T_LeftShiftAssign = "LeftShiftAssign"
    show T_RightShiftAssign = "RightShiftAssign"
    show T_BitAndAssign = "BitAndAssign"
    show T_BitXorAssign = "BitXorAssign"
    show T_BitOrAssign = "BitOrAssign"
    show T_Comma = "Comma"

operators :: [String]
operators =
    [
        "++",
        "--",
        "()",
        "[]",
        ".",
        "->",
        "()",
        "++",
        "--",
        "+",
        "-",
        "!",
        "~",
        "()",
        "*",
        "&",
        "sizeof",
        "_Alignof",
        "*",
        "/",
        "%",
        "+",
        "-",
        "<<",
        ">>",
        "<",
        "<=",
        ">",
        ">=",
        "==",
        "!=",
        "&",
        "^",
        "|",
        "&&",
        "||",
        "?:",
        "=",
        "+=",
        "-=",
        "*=",
        "/=",
        "%=",
        "<<=",
        ">>=",
        "&=",
        "^=",
        "|=",
        ","
    ]

operatorPrecedence :: OperatorType -> Int
operatorPrecedence op = case op of
    T_SuffixIncrement -> 1
    T_SuffixDecrement -> 1
    T_FunctionCall -> 1
    T_ArrayAccess -> 1
    T_MemberAccess -> 1
    T_MemberAccessThroughPointer -> 1
    T_CompoundLiteral -> 1
    T_PrefixIncrement -> 1
    T_PrefixDecrement -> 1
    T_UnaryPlus -> 2
    T_UnaryMinus -> 2
    T_LogicalNot -> 2
    T_BitNot -> 2
    T_Cast -> 2
    T_Indirection -> 2
    T_AddressOf -> 2
    T_SizeOf -> 2
    T_AlignOf -> 2
    T_Multiply -> 3
    T_Divide -> 3
    T_Mod -> 3
    T_Add -> 4
    T_Sub -> 4
    T_LeftShift -> 5
    T_RightShift -> 5
    T_Less -> 6
    T_LessEqual -> 6
    T_Greater -> 6
    T_GreaterEqual -> 6
    T_Equal -> 7
    T_NotEqual -> 7
    T_BitAnd -> 8
    T_BitXor -> 9
    T_BitOr -> 10
    T_LogicalAnd -> 11
    T_LogicalOr -> 12
    T_TernaryConditional -> 13
    T_Assign -> 14
    T_AddAssign -> 14
    T_SubAssign -> 14
    T_MulAssign -> 14
    T_DivAssign -> 14
    T_ModAssign -> 14
    T_LeftShiftAssign -> 14
    T_RightShiftAssign -> 14
    T_BitAndAssign -> 14
    T_BitXorAssign -> 14
    T_BitOrAssign -> 14
    T_Comma -> 15

data KeywordType =
    T_If
    | T_Else
    | T_While
    | T_For
    | T_Break
    | T_Continue
    | T_Return
    | T_Function
    | T_Var
    | T_True
    | T_False

keywords :: [String]
keywords = ["if", "else", "while", "for", "break", "continue", "return", "function", "var", "true", "false"]

-- String -> KeywordType
keyWordTypeFromString :: String -> KeywordType
keyWordTypeFromString str = case str of
    "if" -> T_If
    "else" -> T_Else
    "while" -> T_While
    "for" -> T_For
    "break" -> T_Break
    "continue" -> T_Continue
    "return" -> T_Return
    "function" -> T_Function
    "var" -> T_Var
    "true" -> AST.T_True
    "false" -> AST.T_False
    _ -> error "not a keyword"

instance Show KeywordType where
    show T_If = "If"
    show T_Else = "Else"
    show T_While = "While"
    show T_For = "For"
    show T_Break = "Break"
    show T_Continue = "Continue"
    show T_Return = "Return"
    show T_Function = "Function"
    show T_Var = "Var"
    show AST.T_True = "True"
    show AST.T_False = "False"


data Token = Token{
    tokenType :: TokenType,
    tokenValue :: String
}
instance Show Token where
    show (Token t v) = "{" ++ show t ++ ": " ++ v ++ "}"