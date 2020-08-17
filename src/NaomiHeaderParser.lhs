> {-# LANGUAGE OverloadedStrings #-}

This is a literate Haskell file

> module NaomiHeaderParser (parseNaomiHeader, fakeHeader) where

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

> data Mode8M = Not8MMode | Is8MMode
>   deriving (Eq, Ord, Show)

> data ModeG1Init = NoInit | WithInit ByteString
>   deriving (Eq, Ord, Show)

> data HeaderEncryption = IsEncrypted | NotEncrypted
>   deriving (Eq, Ord, Show)

> data CoinSettings = ManualCoinSettings { coin1Rate :: Int8
>                                        , coin2Rate :: Int8
>                                        , creditRate :: Int8
>                                        , bonusRate :: Int8
> } deriving Show

> data CoinSetting = Standard Int8 | Manual CoinSettings
>   deriving Show

> data EepromInit = BiosDefaults | 
>   EepromSettings { systemSettings :: Int8
>                  , coinChuteType :: Int8
>                  , coinSetting :: CoinSetting
>                  , seqTextOffset :: ByteString
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
>                           , mode8m      :: Mode8M
>                           , modeG1Bus   :: ModeG1Init
>                           , mXchksum    :: ByteString
>                           , eepromInit  :: [EepromInit]
>                           , seqText     :: ByteString
>                           , mainLdEnts  :: [LoadEntry]
>                           , testLdEnts  :: [LoadEntry]
>                           , mainEntry   :: Int32
>                           , testEntry   :: Int32
>                           , supRegions  :: Int8
>                           , supPlayers  :: Int8
>                           , supDispFqs  :: Int8
>                           , supDispOrs  :: Int8
>                           , chkSerEep   :: Int8
>                           , coinSvcType :: Int8
>                           , m1RomChksum :: ByteString
>                           , hdrEncFlag  :: HeaderEncryption
> } deriving Show

> parseNaomiHeader :: ByteString -> Either String NaomiHeader
> parseNaomiHeader c = DS.runGet parseMetadata c 

> parseMetadata :: DS.Get NaomiHeader
> parseMetadata = do
>   naomiHeader  <- parseSpacePaddedString 16
>   publisher'   <- parseSpacePaddedString 32
>   game1'       <- parseSpacePaddedString 32
>   game2'       <- parseSpacePaddedString 32
>   game3'       <- parseSpacePaddedString 32
>   game4'       <- parseSpacePaddedString 32
>   game5'       <- parseSpacePaddedString 32
>   dummy1'      <- parseSpacePaddedString 32
>   dummy2'      <- parseSpacePaddedString 32
>   dummy3'      <- parseSpacePaddedString 32
>   year'        <- DS.getByteString 2
>   month'       <- DS.getByteString 1
>   day'         <- DS.getByteString 1
>   serial'      <- DS.getByteString 4
>   mode8M'      <- DS.getInt16le
>   modeG1Bus'   <- DS.getInt16le
>   g1BusInit'   <- DS.getByteString (4*8)
>   mXchksum'    <- DS.getByteString 132
>   eepromInit'  <- parseEepromInits
>   seqText'     <- DS.getByteString (32*8)
>   mainLdEnts'  <- parseLoadEntries
>   testLdEnts'  <- parseLoadEntries
>   mainEntPt'   <- DS.getInt32le
>   testEntPt'   <- DS.getInt32le
>   supRegions'  <- DS.getInt8
>   supPlayers'  <- DS.getInt8
>   supDispFqs'  <- DS.getInt8
>   supDispOrs'  <- DS.getInt8
>   chkSerEep'   <- DS.getInt8
>   coinSvcType' <- DS.getInt8
>   m1RomChksum' <- DS.getByteString 138
>   DS.skip 71      -- unused padding bytes
>   hdrEncFlag'  <- DS.getInt8
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
>     (getG1BusMode modeG1Bus' g1BusInit')
>     (mXchksum')
>     (eepromInit')
>     (seqText')
>     (mainLdEnts')
>     (testLdEnts')
>     (mainEntPt')
>     (testEntPt')
>     (supRegions')
>     (supPlayers')
>     (supDispFqs')
>     (supDispOrs')
>     (chkSerEep')
>     (coinSvcType')
>     (m1RomChksum')
>     (getHeaderEncryption hdrEncFlag')

> parseSpacePaddedString :: Int -> DS.Get ByteString
> parseSpacePaddedString l = do
>   x <- DS.getByteString l
>   pure $ stripByteString x

> parseLoadEntries :: DS.Get [LoadEntry]
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

> parseLoadEntry :: DS.Get LoadEntry
> parseLoadEntry = do
>   romOffset'   <- DS.getInt32le 
>   ramAddress'  <- DS.getInt32le
>   entryLength' <- DS.getInt32le
>   pure $ Entry (romOffset') (ramAddress') (entryLength')

> parseEepromInits :: DS.Get [EepromInit]
> parseEepromInits = do
>   init1' <- parseEepromInit
>   init2' <- parseEepromInit
>   init3' <- parseEepromInit
>   init4' <- parseEepromInit
>   init5' <- parseEepromInit
>   init6' <- parseEepromInit
>   init7' <- parseEepromInit
>   init8' <- parseEepromInit
>   pure $ [init1', init2', init3', init4', init5', init6', init7', init8']

> parseEepromInit :: DS.Get EepromInit
> parseEepromInit = do
>   apply'      <- DS.getInt8
>   system'     <- DS.getInt8
>   chute'      <- DS.getInt8
>   coin'       <- DS.getInt8
>   coin1rate'  <- DS.getInt8
>   coin2rate'  <- DS.getInt8
>   creditrate' <- DS.getInt8
>   bonusrate'  <- DS.getInt8
>   seqtextoff' <- DS.getByteString 8
>   pure $ constructEepromInit apply' system' chute' coin' coin1rate' coin2rate' creditrate' bonusrate' seqtextoff'

> constructEepromInit :: Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> ByteString -> EepromInit
> constructEepromInit 0 _ _ _ _ _ _ _ _ = BiosDefaults
> constructEepromInit _ system chute coin coin1rate coin2rate creditrate bonusrate seqtextoff = EepromSettings system chute (mkCoinSetting coin coin1rate coin2rate creditrate bonusrate) seqtextoff

> mkCoinSetting :: Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> CoinSetting
> mkCoinSetting c c1rate c2rate credrate bonusrate 
>   | 0 <= c && c <= 27 = Standard c
>   | otherwise         = Manual $ ManualCoinSettings c1rate c2rate credrate bonusrate

> stripByteString :: ByteString -> ByteString
> stripByteString = (TE.encodeUtf8 . T.stripEnd . TE.decodeUtf8)

> readInt :: ByteString -> Int
> readInt = B.foldl' addup 0
>   where addup acc x = acc * 10 + (fromIntegral x - 48) 

> get8mMode :: Int16 -> Mode8M
> get8mMode m = do 
>  if m /= 0
>    then Is8MMode
>  else Not8MMode

> getG1BusMode :: Int16 -> ByteString -> ModeG1Init
> getG1BusMode m g1init = do
>   if m /= 0
>     then WithInit g1init
>   else NoInit

> getHeaderEncryption :: Int8 -> HeaderEncryption
> getHeaderEncryption m = 
>   if m /= 0
>     then IsEncrypted
>   else NotEncrypted

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
