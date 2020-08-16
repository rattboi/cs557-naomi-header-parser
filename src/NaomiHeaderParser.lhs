> {-# LANGUAGE OverloadedStrings #-}

This is a literate Haskell file

> module NaomiHeaderParser (parseNaomiHeader, fakeHeader) where

> import Control.Applicative
> import qualified Data.Attoparsec.ByteString as BS
> import Data.ByteString as B hiding (map, pack, take, length)
> import qualified Data.ByteString.Char8 as BC hiding (readInt)
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Data.Text.Encoding as TE
> import qualified Data.Serialize as DS
> import Data.Int
> default (ByteString)

> data LoadEntry = Entry { romOffset   :: Int32
>                        , ramAddress  :: Int32
>                        , entryLength :: Int32
> } deriving Show

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
>                           , modeG1Init  :: ByteString
>                           , mXchksum    :: ByteString
>                           , eepromInit  :: ByteString
>                           , seqText     :: ByteString
>                           , mainLdEnts  :: [LoadEntry]
>                           , testLdEnts  :: [LoadEntry]
>                           , mainEntry   :: Int
>                           , testEntry   :: Int
>                           , supRegions  :: Int
>                           , supPlayers  :: Int
>                           , supDispFqs  :: Int
>                           , supDispOrs  :: Int
>                           , chkSerEep   :: Int
>                           , coinSvcType :: Int
>                           , m1RomChksum :: ByteString
>                           , hdrEncFlag  :: Bool
> } deriving Show

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
>   publisher'   <- parse32ByteSpacePaddedString
>   game1'       <- parse32ByteSpacePaddedString
>   game2'       <- parse32ByteSpacePaddedString
>   game3'       <- parse32ByteSpacePaddedString
>   game4'       <- parse32ByteSpacePaddedString
>   game5'       <- parse32ByteSpacePaddedString
>   dummy1'      <- parse32ByteSpacePaddedString
>   dummy2'      <- parse32ByteSpacePaddedString
>   dummy3'      <- parse32ByteSpacePaddedString
>   year'        <- BS.take 2
>   month'       <- BS.take 1
>   day'         <- BS.take 1
>   serial'      <- BS.take 4
>   mode8M'      <- BS.take 2
>   modeG1Bus'   <- BS.take 2
>   g1BusInit'   <- BS.take (4*8)
>   mXchksum'    <- BS.take 132
>   eepromInit'  <- BS.take (16*8)
>   seqText'     <- BS.take (32*8)
>   mainLdEnts'  <- parseLoadEntries
>   testLdEnts'  <- parseLoadEntries
>   mainEntPt'   <- BS.take 4
>   testEntPt'   <- BS.take 4
>   supRegions'  <- BS.take 1
>   supPlayers'  <- BS.take 1
>   supDispFqs'  <- BS.take 1
>   supDispOrs'  <- BS.take 1
>   chkSerEep'   <- BS.take 1
>   coinSvcType' <- BS.take 1
>   m1RomChksum' <- BS.take 138
>   _            <- BS.take 71      -- unused padding bytes
>   hdrEncFlag'  <- BS.take 1
>   pure $ Header
>     (publisher')
>     (game1')
>     (game2')
>     (game3')
>     (game4')
>     (game5')
>     (dummy1')
>     (dummy2')
>     (dummy3')
>     (readInt year')
>     (readInt month')
>     (readInt day')
>     (serial')
>     (get8mMode mode8M')
>     (getG1BusMode modeG1Bus')
>     (g1BusInit')
>     (mXchksum')
>     (eepromInit')
>     (seqText')
>     (mainLdEnts')
>     (testLdEnts')
>     (readInt mainEntPt')
>     (readInt testEntPt')
>     (readInt supRegions')
>     (readInt supPlayers')
>     (readInt supDispFqs')
>     (readInt supDispOrs')
>     (readInt chkSerEep')
>     (readInt coinSvcType')
>     (m1RomChksum')
>     (getHeaderEncryption hdrEncFlag')
>
> parse32ByteSpacePaddedString :: BS.Parser ByteString
> parse32ByteSpacePaddedString = do
>   x <- BS.take 32
>   pure $ stripByteString x

>
> parseLoadEntries :: BS.Parser [LoadEntry]
> parseLoadEntries = do
>   entry1' <- parseLoadEntry
>   entry2' <- parseLoadEntry
>   entry3' <- parseLoadEntry
>   entry4' <- parseLoadEntry
>   entry5' <- parseLoadEntry
>   entry6' <- parseLoadEntry
>   entry7' <- parseLoadEntry
>   entry8' <- parseLoadEntry
>   pure $ [entry1'
>         , entry2'
>         , entry3'
>         , entry4'
>         , entry5'
>         , entry6'
>         , entry7'
>         , entry8' ]

> parseLoadEntry :: BS.Parser LoadEntry
> parseLoadEntry = do
>   romOffset'   <- BS.take 4
>   ramAddress'  <- BS.take 4
>   entryLength' <- BS.take 4
>   pure $ Entry (readInt32 romOffset') (readInt32 ramAddress') (readInt32 entryLength')
>
>
> stripByteString :: ByteString -> ByteString
> stripByteString = (TE.encodeUtf8 . T.stripEnd . TE.decodeUtf8)

> readInt :: ByteString -> Int
> readInt = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 

> readInt32 :: ByteString -> Int32
> readInt32 = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 


> checkBytes :: [Char] -> (ByteString -> Bool)
> checkBytes test = (/= BC.pack test)

> get8mMode :: ByteString -> Bool
> get8mMode = checkBytes [C.chr 0, C.chr 0]

> getG1BusMode :: ByteString -> Bool
> getG1BusMode = checkBytes [C.chr 0, C.chr 0]

> getHeaderEncryption :: ByteString -> Bool
> getHeaderEncryption = checkBytes [C.chr 0]

> spacePadStr :: String -> Int -> String
> spacePadStr s l = s ++ (take (l - length s) (repeat ' '))

> fakeBytes :: Int -> String
> fakeBytes n = [C.chr 0 | _ <- [1..n]]

> fakeEntry :: Int32 -> Int32 -> Int32 -> String
> fakeEntry rom ram eLen = BC.unpack $
>   (DS.encode rom) `BC.append`
>   (DS.encode ram) `BC.append`
>   (DS.encode eLen)

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
>   fakeBytes 2 ++
>   fakeBytes 2 ++
>   fakeBytes (8 * 4) ++                     -- G1 Bus Initvals, if G1Bus mode is non-zero
>   fakeBytes 132 ++                         -- M2/M4-type rom checksum
>   fakeBytes (8 * 16) ++                    -- EEPROM init values (16 bytes per region, 8 regions)
>   (spacePadStr "" (8 * 32)) ++             -- Sequence Text (32 bytes per region, 8 regions)
>   (fakeEntry 1234 5678 4096) ++
>   fakeBytes (3*4*7) ++                     -- Up to 8x Game load entries (3-tuples of 4 bytes each)
>   (fakeEntry 5555 6666 4096) ++
>   fakeBytes (3*4*7) ++                     -- Up to 8x Test load entries (3-tuples of 4 bytes each)
>   fakeBytes 4 ++                           -- Main entrypoint in RAM
>   fakeBytes 4 ++                           -- Test entrypoint in RAM
>   fakeBytes 1 ++                           -- Supported Regions (bitfield)
>   fakeBytes 1 ++                           -- Supported # of players (bitfield)
>   fakeBytes 1 ++                           -- Supported display frequency (bitfield)
>   fakeBytes 1 ++                           -- Supported display orientation (bitfield)
>   fakeBytes 1 ++                           -- Check rom board serial number eeprom? (yes if 1)
>   fakeBytes 1 ++                           -- Coin service type (0 = common, 1 = individual)
>   (spacePadStr "" 138) ++                  -- M1-type rom checksums
>   (spacePadStr "" 71) ++                   -- Unused padding
>   fakeBytes 1                              -- Header Encryption flag
