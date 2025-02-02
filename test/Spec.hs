
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
                Right (_, ident) -> do
                    print ident

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

tokenizeKeywordTest :: IO()
tokenizeKeywordTest = do
    let str = "if else while for"
    let res = tokenize str
    print res
    print "hello"
    print res

tokenizeFileTest :: IO ()
tokenizeFileTest = do
    let path = "test/test.txt"
    source <- readFile path
    let res = tokenize source
    print res

main :: IO ()
main = tokenizeFileTest
