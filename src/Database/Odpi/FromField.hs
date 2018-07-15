{-# language
    LambdaCase
  , ScopedTypeVariables
#-}
module Database.Odpi.FromField where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Fixed
import Data.Int
import Data.Proxy
import Data.Word
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime

import Database.Odpi.NativeValue
import Database.Odpi.LibDpi
import Database.Odpi.Types

convError :: String -> NativeValue -> IO a
convError s v = throwIO $ DpiConversionError $ "expected " ++ s ++ " but got " ++ show v

-- | A type that may be converted from dpiData
class FromField a where
  fromField :: NativeValue -> IO a
  nativeTypeFor :: Proxy a -> Maybe NativeTypeNum
  nativeTypeFor _ = Nothing

instance FromField Bool where
  fromField (NativeBool b) = pure b
  fromField v = convError "Bool" v

instance FromField Char where
  fromField (NativeBytes b) = pure $ B8.head b
  fromField v = convError "Char" v

instance FromField ByteString where
  fromField (NativeBytes b) = pure b
  fromField v = convError "ByteString" v

instance FromField T.Text where
  fromField = fmap TE.decodeUtf8 . fromField

instance FromField Int where
  fromField = fmap (fromIntegral :: Int64 -> Int) . fromField

instance FromField Int16 where
  fromField = fmap (fromIntegral :: Int64 -> Int16) . fromField

instance FromField Int32 where
  fromField = fmap (fromIntegral :: Int64 -> Int32) . fromField

instance FromField Int64 where
  fromField (NativeInt64 x) = pure x
  fromField (NativeDouble x) = pure $ truncate x
  fromField v = convError "Int64" v

instance FromField Word where
  fromField = fmap (fromIntegral :: Word64 -> Word) . fromField

instance FromField Word16 where
  fromField = fmap (fromIntegral :: Word64 -> Word16) . fromField

instance FromField Word32 where
  fromField = fmap (fromIntegral :: Word64 -> Word32) . fromField

instance FromField Word64 where
  fromField (NativeUint64 x) = pure x
  fromField (NativeDouble x) = pure $ truncate x
  fromField v = convError "Word64" v

instance FromField Float where
  fromField (NativeFloat x) = pure x
  fromField (NativeDouble x) = pure $ realToFrac x
  fromField v = convError "Float" v

instance FromField Double where
  fromField (NativeDouble x) = pure x
  fromField (NativeFloat x) = pure $ realToFrac x
  fromField v = convError "Double" v

-- Scientific is fetched as bytes which is the only way to preserve full precision of
-- Oracle NUMBER type. Beware of using it haphazardly as this incures a performance
-- penalty. Always use Double unless you really need additional precision.
instance FromField Scientific where
  fromField (NativeBytes x) = pure $ read $ B8.unpack x
  fromField v = convError "Scientific" v
  nativeTypeFor _ = Just NativeTypeBytes

instance FromField LocalTime where
  fromField (NativeTimestamp x) = do
    let d = fromGregorian (fromIntegral $ timestamp_year x)
                          (fromIntegral $ timestamp_month x)
                          (fromIntegral $ timestamp_day x)
        sec = fromIntegral $ timestamp_second x :: Pico
        fsec = MkFixed $ (fromIntegral $ timestamp_fsecond x) `div` 10
        t = TimeOfDay (fromIntegral $ timestamp_hour x)
                      (fromIntegral $ timestamp_minute x)
                      (sec + fsec)
    pure $ LocalTime d t
  fromField v = convError "LocalTime" v

instance FromField a => FromField (Maybe a) where
  fromField (NativeNull _) = pure Nothing
  fromField v = Just <$> fromField v
  nativeTypeFor _ = nativeTypeFor (Proxy :: Proxy a)
