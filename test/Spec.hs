
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Parser(Parser, left,runParser,ppair,  getNumeric, identifier,tokenize)

gl :: (a,b) -> a
gl (a,_) = a

gr :: (a,b) -> b
gr (_,b) = b

basicTest :: IO()
basicTest = do
    let str = "1234Abc"
    let resNum = runParser getNumeric str

    print resNum
    case resNum of
        Left err -> print err
        Right (rest, num) -> do
            print num
            let resId = runParser identifier rest
            case resId of
                Left err -> print err
                Right (rest', id) -> do
                    print id

pairBasicTest :: IO()
pairBasicTest = do
    let str = "1234Abc"
    let res = runParser (ppair getNumeric identifier) str
    print res

tokenizeTest :: IO()
tokenizeTest = do
    let str = "1234 + 5678 asbd123bd de _a _as_12_d "
    let res = tokenize str
    print res

main :: IO ()
main = tokenizeTest
