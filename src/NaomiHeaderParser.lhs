> {-# LANGUAGE OverloadedStrings #-}

Written by Bradon Kanyid over the course of a week
CS557

> module NaomiHeaderParser (parseNaomiHeader, fakeHeader, printNaomiHeader) where

> import Data.ByteString as B hiding (map, pack, take, length)
> import qualified Data.ByteString.Char8 as BC hiding (readInt)
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Data.Text.Encoding as TE
> import qualified Data.Serialize as DS
> import Data.Int
> import Data.Bits
> default (ByteString)

> data LoadEntry = Entry { romOffset   :: Int32
>                        , ramAddress  :: Int32
>                        , entryLength :: Int32
> } 

> instance Show LoadEntry where
>   show (Entry rom ram eLen) = 
>     "\n" ++
>     "            Rom Offset: " ++ show rom ++ "\n" ++
>     "            Ram Address: " ++ show ram ++ "\n" ++
>     "            Entry Length: " ++ show eLen

> data Mode8M = Not8MMode | Is8MMode
>   deriving (Eq, Ord, Show)

> data ModeG1Init = NoInit | WithInit ByteString
>   deriving (Eq, Ord, Show)

> data EepromSerialCheck = NoCheckEepromSerial | CheckEepromSerial
>   deriving (Eq, Ord, Show)

> data CoinService = Common | Individual
>   deriving (Eq, Ord, Show)

> data HeaderEncryption = IsEncrypted | NotEncrypted
>   deriving (Eq, Ord, Show)

> data CoinSettings = ManualCoinSettings { coin1Rate :: Int8
>                                        , coin2Rate :: Int8
>                                        , creditRate :: Int8
>                                        , bonusRate :: Int8
> } 

> instance Show CoinSettings where
>   show (ManualCoinSettings c1rate c2rate crdRate bRate) = 
>     "\n" ++
>     "            Coin 1 Rate: " ++ show c1rate ++ "\n" ++
>     "            Coin 2 Rate: " ++ show c2rate++ "\n" ++
>     "            Credit Rate: " ++ show crdRate ++ "\n" ++
>     "            Bonus Rate : " ++ show bRate


> data CoinSetting = Standard Int8 | Manual CoinSettings

> instance Show CoinSetting where
>   show (Standard s) = "\n          Built-in Setting: " ++ show s
>   show (Manual m)   = "\n          Manual Setting: " ++ show m


> data EepromInit = BiosDefaults | 
>   EepromSettings { systemSettings :: Int8
>                  , coinChuteType :: Int8
>                  , coinSetting :: CoinSetting
>                  , seqTextOffset :: ByteString
> } 

> instance Show EepromInit where
>   show (BiosDefaults) = "\n    BiosDefaults"
>   
>   show (EepromSettings s chute coin seq) = 
>     "\n    {\n" ++ 
>     "      System Settings: " ++ show s ++ "\n" ++
>     "      Coin Chute Type: " ++ show chute ++ "\n" ++
>     "        Coin Settings: " ++ show coin ++ "\n" ++
>     "      Seq Text Offset: " ++ show seq ++ "\n" ++
>     "    }"


> data RegionSupport = Regions { japan :: Bool
>                              , usa :: Bool
>                              , export :: Bool
>                              , korea :: Bool
>                              , australia :: Bool
> }

> instance Show RegionSupport where
>   show (Regions j u e k a) = 
>     "{\n" ++ 
>     "                Japan: " ++ show j ++ "\n" ++
>     "                   US: " ++ show u ++ "\n" ++
>     "                  Exp: " ++ show e ++ "\n" ++
>     "                  Kor: " ++ show k ++ "\n" ++
>     "                  Aus: " ++ show a ++ "\n" ++
>     "              }"

> data PlayerSupport = Players { player1 :: Bool
>                              , player2 :: Bool
>                              , player3 :: Bool
>                              , player4 :: Bool
> }

> instance Show PlayerSupport where
>   show (Players p1 p2 p3 p4) = 
>     "{\n" ++ 
>     "                1-Player: " ++ show p1 ++ "\n" ++
>     "                2-Player: " ++ show p2 ++ "\n" ++
>     "                3-Player: " ++ show p3 ++ "\n" ++
>     "                4-Player: " ++ show p4 ++ "\n" ++
>     "              }"

> data FrequencySupport = Frequencies { freq31khz:: Bool
>                                     , freq15khz:: Bool
> }

> instance Show FrequencySupport where
>   show (Frequencies f1 f2) = 
>     "{\n" ++ 
>     "                31KHz: " ++ show f1 ++ "\n" ++
>     "                15KHz: " ++ show f2 ++ "\n" ++
>     "              }"

> data OrientationSupport = Orientations { horizontal :: Bool
>                                        , vertical   :: Bool
> }

> instance Show OrientationSupport where
>   show (Orientations f1 f2) = 
>     "{\n" ++ 
>     "                Horizontal: " ++ show f1 ++ "\n" ++
>     "                  Vertical: " ++ show f2 ++ "\n" ++
>     "              }"

> data RomChecksum = NoChecksum | Checksum ByteString

> instance Show RomChecksum where
>   show (Checksum bytes) = show bytes
>   show (NoChecksum) = "NoChecksum"

> data NaomiHeader = Header { publisher :: ByteString
>                           , japGameName :: ByteString
>                           , usaGameName :: ByteString
>                           , expGameName :: ByteString
>                           , korGameName :: ByteString
>                           , ausGameName :: ByteString
>                           , rg6GameName :: ByteString
>                           , rg7GameName :: ByteString
>                           , rg8GameName :: ByteString
>                           , dateYear    :: Int16
>                           , dateMonth   :: Int8
>                           , dateDay     :: Int8
>                           , serialNum   :: ByteString
>                           , mode8m      :: Mode8M
>                           , modeG1Bus   :: ModeG1Init
>                           , mXchksum    :: RomChecksum
>                           , eepromInit  :: [EepromInit]
>                           , seqText     :: ByteString
>                           , mainLdEnts  :: [LoadEntry]
>                           , testLdEnts  :: [LoadEntry]
>                           , mainEntry   :: Int32
>                           , testEntry   :: Int32
>                           , supRegions  :: RegionSupport
>                           , supPlayers  :: PlayerSupport
>                           , supDispFqs  :: FrequencySupport 
>                           , supDispOrs  :: OrientationSupport
>                           , chkSerEep   :: EepromSerialCheck
>                           , coinSvcType :: CoinService
>                           , m1RomChksum :: RomChecksum
>                           , hdrEncFlag  :: HeaderEncryption
> }

> instance Show NaomiHeader where
>   show (Header p jgn usgn exgn krgn augn _ _ _ year month day serial mode8 modeg1 mXchksum eepromInit seqtext mainLdEnts testLdEnts mainEntry testEntry supRegions supPlayers supDispFqs supDispOrs chkSerEep coinSvcType m1RomChksum hdrEncFlag) = 
>     "{\n" ++ 
>     "  Publisher: " ++ show p ++ "\n" ++
>     "  Regional Game Names: {" ++ "\n" ++ 
>     "    Japan: " ++ show jgn ++ "\n" ++ 
>     "       US: " ++ show usgn ++ "\n" ++ 
>     "      Exp: " ++ show exgn ++ "\n" ++ 
>     "      Kor: " ++ show krgn ++ "\n" ++ 
>     "      Aus: " ++ show augn ++ "\n" ++
>     "  }\n" ++
>     "  Mfr Date: " ++ show month ++ "/" ++ show day ++ "/" ++ show year ++ "\n" ++
>     "  Serial #: " ++ show serial ++ "\n" ++
>     "  8Mb Mode: " ++ show mode8 ++ "\n" ++
>     "  G1 Bus Mode: " ++ show modeg1 ++ "\n" ++
>     "  M2/M4 Checksum: " ++ show mXchksum ++ "\n" ++
>     "  Eeprom Init Vals: " ++ show eepromInit ++ "\n" ++
>     "  Sequence Text: " ++ show seqtext ++ "\n" ++
>     "  Main Load Entries: " ++ show mainLdEnts ++ "\n" ++
>     "  Test Load Entries: " ++ show testLdEnts ++ "\n" ++
>     "  Main Entrypoint: " ++ show mainEntry ++ "\n" ++
>     "  Test Entrypoint: " ++ show testEntry ++ "\n" ++
>     "  Supported Options: {" ++ "\n" ++
>     "              Regions: " ++ show supRegions ++ "\n" ++
>     "              Players: " ++ show supPlayers ++ "\n" ++
>     "              Disp Freqs: " ++ show supDispFqs ++ "\n" ++
>     "              Disp Orients: " ++ show supDispOrs ++ "\n" ++
>     "  }\n" ++
>     "  Check EEPROM Serial: " ++ show chkSerEep ++ "\n" ++
>     "  Coin Service Type: " ++ show coinSvcType ++ "\n" ++
>     "  M1 Checksum: " ++ show m1RomChksum ++ "\n" ++
>     "  Header Encryption: " ++ show hdrEncFlag ++ "\n" ++
>     "\n"

> printNaomiHeader :: Either String NaomiHeader -> IO ()
> printNaomiHeader header = case header of
>   Left s -> B.putStr $ BC.pack s
>   Right nh -> B.putStr $ BC.pack (show nh)

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
>   year'        <- DS.getInt16le
>   month'       <- DS.getInt8
>   day'         <- DS.getInt8
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
>     (year')
>     (month')
>     (day')
>     (serial')
>     (get8mMode mode8M')
>     (getG1BusMode modeG1Bus' g1BusInit')
>     (getChecksum mXchksum')
>     (eepromInit')
>     (seqText')
>     (mainLdEnts')
>     (testLdEnts')
>     (mainEntPt')
>     (testEntPt')
>     (getSupportedRegions supRegions')
>     (getSupportedPlayers supPlayers')
>     (getSupportedFrequencies supDispFqs')
>     (getSupportedOrientations supDispOrs')
>     (getCheckEepromSerial chkSerEep')
>     (getCoinService coinSvcType')
>     (getChecksum m1RomChksum')
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

> get8mMode :: Int16 -> Mode8M
> get8mMode m
>   | m /= 0    = Is8MMode
>   | otherwise = Not8MMode

> getG1BusMode :: Int16 -> ByteString -> ModeG1Init
> getG1BusMode m g1init 
>   | m /= 0    = WithInit g1init
>   | otherwise = NoInit

> getCheckEepromSerial :: Int8 -> EepromSerialCheck
> getCheckEepromSerial m 
>   | m == 0    = NoCheckEepromSerial
>   | otherwise = CheckEepromSerial

> getCoinService :: Int8 -> CoinService
> getCoinService c 
>   | c == 0    = Common
>   | otherwise = Individual

> getHeaderEncryption :: Int8 -> HeaderEncryption
> getHeaderEncryption m 
>   | m /= -128 = IsEncrypted
>   | otherwise = NotEncrypted

> getChecksum :: ByteString -> RomChecksum
> getChecksum bytes = case (B.all (== 0) bytes) of
>   True  -> NoChecksum
>   False -> Checksum bytes

> getSupportedRegions :: Int8 -> RegionSupport
> getSupportedRegions r = Regions 
>   (testBit r 0) 
>   (testBit r 1) 
>   (testBit r 2) 
>   (testBit r 3) 
>   (testBit r 4)

> getSupportedPlayers :: Int8 -> PlayerSupport
> getSupportedPlayers p 
>  | p == 0    = Players True True True True
>  | otherwise = Players
>    (testBit p 0) 
>    (testBit p 1) 
>    (testBit p 2) 
>    (testBit p 3) 

> getSupportedFrequencies :: Int8 -> FrequencySupport
> getSupportedFrequencies f 
>  | f == 0    = Frequencies True True
>  | otherwise = Frequencies
>    (testBit f 0) 
>    (testBit f 1) 

> getSupportedOrientations :: Int8 -> OrientationSupport
> getSupportedOrientations o 
>  | o == 0    = Orientations True True
>  | otherwise = Orientations
>    (testBit o 0) 
>    (testBit o 1) 

> spacePadStr :: String -> Int -> String
> spacePadStr s l = s ++ (take (l - length s) (repeat ' '))

> fakeBytes :: Int -> String
> fakeBytes n = [C.chr 0 | _ <- [1..n]]

> fakeEntry :: Int32 -> Int32 -> Int32 -> String
> fakeEntry rom ram eLen = BC.unpack $
>   (DS.encode rom) `BC.append`
>   (DS.encode ram) `BC.append`
>   (DS.encode eLen)

> fakeEepromInit :: Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> String
> fakeEepromInit apply system chute coin coin1rate coin2rate creditrate bonusrate = BC.unpack $
>   (DS.encode apply) `BC.append`
>   (DS.encode system) `BC.append`
>   (DS.encode chute) `BC.append`
>   (DS.encode coin) `BC.append`
>   (DS.encode coin1rate) `BC.append`
>   (DS.encode coin2rate) `BC.append`
>   (DS.encode creditrate) `BC.append`
>   (DS.encode bonusrate) `BC.append`
>   (DS.encode (0 :: Int64))

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
>   fakeEepromInit 1 5 0 25 0 0 0 0 ++
>   fakeEepromInit 0 0 0 0 0 0 0 0 ++
>   fakeEepromInit 1 6 1 28 1 1 1 1 ++
>   fakeBytes (5 * 16) ++                    -- EEPROM init values (16 bytes per region, 8 regions)
>   (spacePadStr "CREDIT TO START" 32) ++             -- Sequence Text (32 bytes per region, 8 regions)
>   (spacePadStr "CREDIT TO CONTINUE" 32) ++             -- Sequence Text (32 bytes per region, 8 regions)
>   (spacePadStr "" (6 * 32)) ++             -- Sequence Text (32 bytes per region, 8 regions)
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
>   fakeBytes 138 ++                         -- M1-type rom checksums
>   (spacePadStr "" 71) ++                   -- Unused padding
>   fakeBytes 1                              -- Header Encryption flag
