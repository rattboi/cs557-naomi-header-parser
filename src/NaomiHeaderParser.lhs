> {-# LANGUAGE OverloadedStrings #-}

This is a literate Haskell file

> module NaomiHeaderParser (parseNaomiHeader) where

> import Control.Applicative 
> import Data.Attoparsec.ByteString as BS
> import Data.ByteString as B hiding (map)
> import Data.Word (Word8)
> default (ByteString)

> data NaomiHeader = Header { publisher :: String 
>                           , japGameName :: String 
>                           , usaGameName :: String 
>                           , expGameName :: String 
>                           , korGameName :: String 
>                           , ausGameName :: String 
>                           , rg6GameName :: String 
>                           , rg7GameName :: String 
>                           , rg8GameName :: String } 
>   deriving Show

> parseNaomiHeader :: ByteString -> Either ByteString NaomiHeader
> parseNaomiHeader c = 
>   case parse parseMetadata c of
>     Fail {}         -> Left "Wrong header"
>     Partial _       -> Left "Incomplete parse"
>     Done c  r       -> Right r

> parseMetadata :: Parser NaomiHeader
> parseMetadata = do
>   _ <- string "NAOMI" 
>   BS.take 11
>   _ <- newline
>   pure $ Header "Hello" "Game1" "Game2" "Game3" "Game4" "Game5" "Game6" "Game7" "Game8"

> newline :: Parser Word8
> newline = word8 10

> readInt :: ByteString -> Int
> readInt = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 

