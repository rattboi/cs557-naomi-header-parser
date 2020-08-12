> {-# LANGUAGE OverloadedStrings #-}

This is a literate Haskell file

> module NaomiHeaderParser (parseNaomiHeader) where

> import Control.Applicative 
> import qualified Data.Attoparsec.ByteString as BS
> import Data.ByteString as B hiding (map, pack, take, length)
> import qualified Data.ByteString.Char8 as BC hiding (readInt)
> import qualified Data.Text as T
> import Data.Text.Encoding as TE
> default (ByteString)

> data NaomiHeader = Header { publisher :: ByteString 
>                           , japGameName :: ByteString 
>                           , usaGameName :: ByteString 
>                           , expGameName :: ByteString 
>                           , korGameName :: ByteString 
>                           , ausGameName :: ByteString 
>                           , rg6GameName :: ByteString 
>                           , rg7GameName :: ByteString 
>                           , rg8GameName :: ByteString 
>                           , dateYear    :: Int
>                           , dateMonth   :: Int
>                           , dateDay     :: Int
> } 
>   deriving Show

> parseNaomiHeader :: ByteString -> Either ByteString NaomiHeader
> parseNaomiHeader c = 
>   case BS.parse parseMetadata c of
>     BS.Fail {}         -> Left "Wrong header"
>     BS.Partial _       -> Left "Incomplete parse"
>     BS.Done _  r       -> Right r

> parseMetadata :: BS.Parser NaomiHeader
> parseMetadata = do
>   _ <- BS.string "NAOMI" 
>   _ <- BS.take 11
>   publisher' <- BS.take 32
>   game1'     <- BS.take 32
>   game2'     <- BS.take 32
>   game3'     <- BS.take 32
>   game4'     <- BS.take 32
>   game5'     <- BS.take 32
>   dummy1'    <- BS.take 32
>   dummy2'    <- BS.take 32
>   dummy3'    <- BS.take 32
>   year'      <- BS.take 2
>   month'     <- BS.take 1
>   day'       <- BS.take 1
>   pure $ Header 
>     (stripByteString publisher') 
>     (stripByteString game1') 
>     (stripByteString game2')
>     (stripByteString game3')
>     (stripByteString game4') 
>     (stripByteString game5') 
>     (stripByteString dummy1') 
>     (stripByteString dummy2') 
>     (stripByteString dummy3') 
>     (readInt year')
>     (readInt month')
>     (readInt day')


> stripByteString :: ByteString -> ByteString
> stripByteString = (TE.encodeUtf8 . T.stripEnd . TE.decodeUtf8)

> readInt :: ByteString -> Int
> readInt = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 

> spacePadStr :: String -> Int -> String
> spacePadStr s l = s ++ (take (l - length s) (repeat ' '))

> fakeHeader :: ByteString 
> fakeHeader = BC.pack $ 
>   (spacePadStr "NAOMI" 16) ++
>   (spacePadStr "The Game Publisher" 32) ++
>   (spacePadStr "My Game in Japanese" 32) ++
>   (spacePadStr "My Game in English" 32) ++ 
>   (spacePadStr "My Game in \"Export\"" 32) ++
>   (spacePadStr "My Game in Korea" 32) ++ 
>   (spacePadStr "My Game in Australia" 32) ++
>   (spacePadStr "Dummy Title #1" 32) ++
>   (spacePadStr "Dummy Title #2" 32) ++
>   (spacePadStr "Dummy Title #3" 32) ++
>   ("99") ++
>   ("9") ++
>   ("9")
