module Tables (Table (..), TableFile (..), readTableFile, writeTableFile) where

import CharCode
import qualified Control.Monad.Trans.Class as M
import qualified Control.Monad.Trans.State as M
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Prelude

length32 :: ByteString -> Word32
length32 bs = fromIntegral $ BS.length bs

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
        verify "table alignment" 0 $ mod teOffset 4
        teLength <- getWord32be
        return MkTableEntry{..}

data Table = MkTable
    { tTag :: CharCode
    , tData :: ByteString
    }
    deriving (Eq)

tableToEntry :: Word32 -> Table -> TableEntry
tableToEntry offset MkTable{..} =
    MkTableEntry
        { teTag = tTag
        , teChecksum = calcChecksum tData
        , teOffset = offset
        , teLength = length32 tData
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

pad4Length :: Word32 -> Word32
pad4Length n = ((n + 3) `div` 4) * 4

pad4 :: ByteString -> ByteString
pad4 bs =
    let len = length32 bs
     in bs <> BS.replicate (fromIntegral $ pad4Length len - len) 0

pluckChecksum :: Word32 -> Get (ByteString, Word32)
pluckChecksum offset = do
    bs1 <- getLazyByteString $ (fromIntegral offset) + 8
    checksum <- getWord32be
    bs2 <- getRemainingLazyByteString
    return (bs1 <> BS.replicate 4 0 <> bs2, checksum)

calcChecksum :: ByteString -> Word32
calcChecksum bs =
    let bsp = pad4 bs
        nLongs = length32 bsp `div` 4
        longs = runGet (for [1 .. nLongs] $ \_ -> getWord32be) bsp
     in sum longs

verifyChecksum :: MonadFail m => String -> Word32 -> ByteString -> m ()
verifyChecksum name expected bs = verify (name <> " checksum") expected $ calcChecksum bs

getTable :: TableEntry -> Get (Maybe Word32, Table)
getTable MkTableEntry{..} = do
    skip $ fromIntegral teOffset
    bs <- getLazyByteString $ fromIntegral teLength
    if teTag == toCharCode "head"
        then do
            (bs1, _) <- runGetM bs $ pluckChecksum 0
            verifyChecksum (show teTag) teChecksum bs1
            return (Just teOffset, MkTable teTag bs1)
        else do
            verifyChecksum (show teTag) teChecksum bs
            return (Nothing, MkTable teTag bs)

data TableFile = MkTableFile
    { tfScalerType :: CharCode
    , tfTables :: [Table]
    }
    deriving (Eq)

readTableFile :: MonadFail m => ByteString -> m TableFile
readTableFile bs = do
    (scalerType, entries) <- runGetM bs getEntries
    ctables <- for entries $ \entry -> runGetM bs (getTable entry)
    case mapMaybe fst ctables of
        [fcoffset] -> do
            (bs', fchecksum) <- runGetM bs $ pluckChecksum fcoffset
            verifyChecksum "file" (0xB1B0AFBA - fchecksum) bs'
        [] -> fail "no head table"
        _ -> fail "multiple head tables"
    return $ MkTableFile scalerType $ fmap snd ctables

putFirstTableFile :: TableFile -> PutM Word32
putFirstTableFile MkTableFile{..} = do
    let numTables = fromIntegral $ Prelude.length tfTables
        b = logBase2 numTables
        searchRange = bit b * 16
        entrySelector = fromIntegral b
        rangeShift = numTables * 16 - searchRange
    put tfScalerType
    putWord16be numTables
    putWord16be searchRange
    putWord16be entrySelector
    putWord16be rangeShift
    (_, csoffsets) <-
        M.execStateT
            ( for_ tfTables $ \table -> do
                (offset, csoffsets) <- M.get
                let entry = tableToEntry offset table
                    csoffsets' =
                        if teTag entry == toCharCode "head"
                            then (offset + 8) : csoffsets
                            else csoffsets
                M.lift $ put entry
                M.put $ (offset + pad4Length (teLength entry), csoffsets')
            )
            (12 + (fromIntegral numTables) * 16, [])
    csoffset <- case csoffsets of
        [csoffset] -> return csoffset
        _ -> error "multiple head tables"
    for_ tfTables $ \table -> putLazyByteString $ pad4 $ tData table
    return csoffset

writeTableFile :: TableFile -> ByteString
writeTableFile tf =
    let (csoffset, bs) = runPutM $ putFirstTableFile tf
        fchecksum = 0xB1B0AFBA - calcChecksum bs
        (bs0, bs1) = BS.splitAt (fromIntegral csoffset) bs
        bs2 = BS.drop 4 bs1
     in runPut $ do
            putLazyByteString bs0
            putWord32be fchecksum
            putLazyByteString bs2
