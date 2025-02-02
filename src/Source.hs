{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Source
    ( Source
    , openSource
    , readChar
    , closeSource
    ) where

import System.IO (Handle, IOMode(ReadMode), hClose, hGetChar, hIsEOF, openFile)

data Source = Source Handle

-- 打开源文件
openSource :: FilePath -> IO Source
openSource path = do
    handle <- openFile path ReadMode
    return (Source handle)

-- 读取一个字符
readChar :: Source -> IO (Maybe Char)
readChar (Source handle) = do
    eof <- hIsEOF handle
    if eof
        then return Nothing
        else do
            char <- hGetChar handle
            return (Just char)

-- 关闭源文件
closeSource :: Source -> IO ()
closeSource (Source handle) = hClose handle