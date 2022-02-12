module Tables (Table (..), TableFile (..), readTableFile) where

import CharCode
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Maybe
import Data.Traversable
import Prelude

runGetM :: MonadFail m => ByteString -> Get a -> m a
runGetM bs g = case runGetOrFail g bs of
    Left (_, _, err) -> fail err
    Right (_, _, a) -> return a

verify :: (MonadFail m, Eq a, Show a) => String -> a -> a -> m ()
verify msg expected found =
    if expected == found
        then return ()
        else fail $ msg ++ ": expected " ++ show expected ++ ", found " ++ show found

data TableEntry = MkTableEntry
    { teTag :: CharCode
    , teChecksum :: Word32
    , teOffset :: Word32
    , teLength :: Word32
    }

instance Binary TableEntry where
    put MkTableEntry{..} = do
        put teTag
        putWord32be teChecksum
        putWord32be teOffset
        putWord32be teLength
    get = do
        teTag <- get
        teChecksum <- getWord32be
        teOffset <- getWord32be
        teLength <- getWord32be
        return MkTableEntry{..}

data Table = MkTable
    { tTag :: CharCode
    , tData :: ByteString
    }

logBase2 :: FiniteBits a => a -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

getEntries :: Get (CharCode, [TableEntry])
getEntries = do
    scalerType <- get
    numTables <- getWord16be
    let b = logBase2 numTables
    searchRange <- getWord16be
    verify "searchRange" (bit b * 16) searchRange
    entrySelector <- getWord16be
    verify "entrySelector" (fromIntegral b) entrySelector
    rangeShift <- getWord16be
    verify "rangeShift" (numTables * 16 - searchRange) rangeShift
    entries <- for [1 .. numTables] $ \_ -> get
    return (scalerType, entries)

pad4 :: ByteString -> ByteString
pad4 bs = bs <> (BS.replicate (3 - ((BS.length bs + 3) `mod` 4)) 0)

pluckChecksum :: Word32 -> Get (ByteString, Word32)
pluckChecksum offset = do
    bs1 <- getLazyByteString $ (fromIntegral offset) + 8
    checksum <- getWord32be
    bs2 <- getRemainingLazyByteString
    return (bs1 <> BS.replicate 4 0 <> bs2, checksum)

getChecksum :: MonadFail m => ByteString -> m Word32
getChecksum bs = do
    let bsp = pad4 bs
        nLongs = BS.length bsp `div` 4
    longs <- runGetM bsp $ for [1 .. nLongs] $ \_ -> getWord32be
    return $ sum longs

verifyChecksum :: MonadFail m => Word32 -> ByteString -> m ()
verifyChecksum expected bs = do
    found <- getChecksum bs
    verify "checksum" expected found

getTable :: TableEntry -> Get (Maybe Word32, Table)
getTable MkTableEntry{..} = do
    skip $ fromIntegral teOffset
    bs <- getLazyByteString $ fromIntegral teLength
    if teTag == toCharCode "head"
        then do
            (bs1, _) <- runGetM bs $ pluckChecksum 0
            verifyChecksum teChecksum bs1
            return (Just teOffset, MkTable teTag bs1)
        else do
            verifyChecksum teChecksum bs
            return (Nothing, MkTable teTag bs)

data TableFile = MkTableFile
    { tfScalerType :: CharCode
    , tfTables :: [Table]
    }

readTableFile :: MonadFail m => ByteString -> m TableFile
readTableFile bs = do
    (scalerType, entries) <- runGetM bs getEntries
    ctables <- for entries $ \entry -> runGetM bs (getTable entry)
    case mapMaybe fst ctables of
        [fcoffset] -> do
            (bs', fchecksum) <- runGetM bs $ pluckChecksum fcoffset
            verifyChecksum (0xB1B0AFBA - fchecksum) bs'
        [] -> fail "no head table"
        _ -> fail "multiple head tables"
    return $ MkTableFile scalerType $ fmap snd ctables
