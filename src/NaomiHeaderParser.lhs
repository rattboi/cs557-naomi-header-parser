> {-# LANGUAGE OverloadedStrings #-}

This is a literate Haskell file

> module NaomiHeaderParser (parseNaomiHeader) where

> import Control.Applicative 
> import qualified Data.Attoparsec.ByteString as BS
> import Data.ByteString as B hiding (map, pack, take, length)
> import qualified Data.ByteString.Char8 as BC hiding (readInt)
> import qualified Data.Text as T
> import qualified Data.Char as C
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
>                           , serialNum   :: ByteString
>                           , mode8m      :: Bool
>                           , modeG1Bus   :: Bool
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
>   serial'    <- BS.take 4
>   mode8M'    <- BS.take 2
>   modeG1Bus' <- BS.take 2
>   g1BusInit' <- BS.take (8*4)  -- unused for now
>   mXchksum'  <- BS.take 132    -- unused for now
>   eepromInit'<- BS.take (8*16) -- unused for now
>   seqText'   <- BS.take (8*32) -- unused for now
>   
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
>     (serial')
>     (get8mMode mode8M')
>     (getG1BusMode modeG1Bus')

> stripByteString :: ByteString -> ByteString
> stripByteString = (TE.encodeUtf8 . T.stripEnd . TE.decodeUtf8)

> readInt :: ByteString -> Int
> readInt = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 

> get8mMode :: ByteString -> Bool
> get8mMode = (/= BC.pack [C.chr 0, C.chr 0])

> getG1BusMode :: ByteString -> Bool
> getG1BusMode = get8mMode

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
>   ("9") ++
>   ("1234") ++
>   [C.chr 0, C.chr 0] ++
>   [C.chr 0, C.chr 0] ++
>   (spacePadStr "" (8 * 4)) ++              -- G1 Bus Initvals, if G1Bus mode is non-zero
>   (spacePadStr "" 132) ++                  -- M2/M4-type rom checksum
>   (spacePadStr "" (8 * 16)) ++             -- EEPROM init values (16 bytes per region, 8 regions)
>   (spacePadStr "" (8 * 32)) ++             -- Sequence Text (32 bytes per region, 8 regions)
>   (spacePadStr "" (8 * 12) ++              -- Up to 8x Game load entries (3-tuples of 4 bytes each)
>   (spacePadStr "" (8 * 12) ++              -- Up to 8x Test load entries (3-tuples of 4 bytes each)
>   [C.chr 0, C.chr 0, C.chr 0, C.chr 0] ++  -- Main entrypoint in RAM
>   [C.chr 0, C.chr 0, C.chr 0, C.chr 0] ++  -- Test entrypoint in RAM
>   [C.chr 0] ++                             -- Supported Regions (bitfield)
>   [C.chr 0] ++                             -- Supported # of players (bitfield)
>   [C.chr 0] ++                             -- Supported display frequency (bitfield)
>   [C.chr 0] ++                             -- Supported display orientation (bitfield)
>   [C.chr 0] ++                             -- Check rom board serial number eeprom? (yes if 1)
>   [C.chr 0] ++                             -- Coin service type (0 = common, 1 = individual)
>   (spacePadStr "" 138) ++                  -- M1-type rom checksums
>   (spacePadStr "" 71) ++                   -- Unused padding
>   [C.chr 0]                                -- Header Encryption flag
