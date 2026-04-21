module Lib where

import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255


intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]


replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc chV bytes = mconcat [before, newChar, after]
    where 
        (before,rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC chV
