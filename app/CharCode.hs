module CharCode where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy as BS hiding (putStrLn)
import Prelude

newtype CharCode = MkCharCode Word32 deriving (Eq, Ord)

toCharCode :: String -> CharCode
toCharCode s = MkCharCode $ runGet getWord32be $ pack $ fmap (fromIntegral . fromEnum) s

fromCharCode :: CharCode -> String
fromCharCode (MkCharCode x) = fmap (toEnum . fromIntegral) $ unpack $ runPut $ putWord32be x

instance Show CharCode where
    show cc = "'" <> fromCharCode cc <> "'"

instance Binary CharCode where
    put (MkCharCode x) = putWord32be x
    get = do
        x <- getWord32be
        return $ MkCharCode x
