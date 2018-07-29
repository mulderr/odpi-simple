{-# language
    LambdaCase
  , ScopedTypeVariables
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
#-}
-- | Conversions from ODPI-C NativeValue to Haskell types.
--
-- For number types we strictly follow the principle to never implicity
-- truncate. This means we never convert from a "larger" NativeValue to
-- a "smaller" Haskell type unless we can guarantee it's safe by examining
-- the column definition.
--
-- However, we do not try to correct ODPI-C which by default returns all
-- NUMBERs as one of Int64, Word64, Float or Double. If ODPI-C truncates a
-- number column to one of those types we just go with it.
--
-- None of the types returned by ODPI-C by default preserve the full 38
-- decimal digits of precision provided by NUMBER. To read a NUMBER with
-- full precision use Scientific. The instance for Scientific overrides
-- the defaults and requests the value as bytes.
--
-- This can be problematic in practice because unqualified NUMBER
-- means no restrictions on the value which translates to a floating point
-- with *maximum* precision and normally can only be read either as Double
-- (truncated by ODPI-C) or Scientific. So whenever you can, be more specific
-- for primary keys etc. You may at the very least want to limit the values
-- to integers by saying NUMBER(38). Or even better NUMBER(18) to fit into
-- Int64.
--
-- If you have no influence over the schema and need things to work anyway,
-- you can create a newtype wrapper with an instance that does whatever
-- weird things you require. Say you have a primary key of type NUMBER
-- but want to read is as Int32:
--
-- newtype Pk32 = Pk32 { unPk32 :: Int32 } deriving (Eq, Show)
-- instance FromField Pk32 where
--   fromField _ (NativeInt64 x) = pure $ Pk32 $ fromIntegral x
--   fromField i v = convError "Pk32" i v
--   nativeTypeFor _ = Just NativeTypeInt64
--
-- First we request that Pk32 always be fetched as Int64 which is the
-- closest to what we want and then further truncate the value to Int32
-- ourselves.
--
-- Also see 'Exactly' for common cases.
module Database.Odpi.FromField where

import Control.Exception
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

data DpiConversionError
  = DpiConversionError NativeValue QueryInfo String

instance Exception DpiConversionError
instance Show DpiConversionError where
  show (DpiConversionError v qi hs) = unlines
    [ "Conversion error:"
    , "  value: " ++ show v
    , "  fetched from column: " ++ show qi
    , "  cannot be safely converted to Haskell type: " ++ hs
    ]

convError :: String -> QueryInfo -> NativeValue -> IO a
convError s i v = throwIO $ DpiConversionError v i s

fromIntegralField :: Num b => String -> Int16 -> Int8 -> QueryInfo -> NativeValue -> IO b
fromIntegralField tyName maxPrec scale i v =
  f v maxPrec scale
  where
    f (NativeInt64 x) p s | p <= 9 && s == 0 = pure $ fromIntegral x
    f (NativeUint64 x) p s | p <= 9 && s == 0 = pure $ fromIntegral x
    f _ _ _ = convError tyName i v

-- | A type that may be converted from dpiData
class FromField a where
  fromField :: QueryInfo -> NativeValue -> IO a
  nativeTypeFor :: Proxy a -> Maybe NativeTypeNum
  nativeTypeFor _ = Nothing

instance FromField Bool where
  fromField _ (NativeBool b) = pure b
  fromField i v = convError "Bool" i v

instance FromField Char where
  fromField _ (NativeBytes b) = pure $ B8.head b
  fromField i v = convError "Char" i v

instance FromField ByteString where
  fromField _ (NativeBytes b) = pure b
  fromField i v = convError "ByteString" i v

instance FromField T.Text where
  fromField i = fmap TE.decodeUtf8 . fromField i

-- Int is rather vague by definition:
-- A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]
intDecimalPrec :: Int16
intDecimalPrec = truncate $ (logBase 10 $ realToFrac (maxBound :: Int) :: Double)

wordDecimalPrec :: Int16
wordDecimalPrec = truncate $ (logBase 10 $ realToFrac (maxBound :: Word) :: Double)

instance FromField Int where
  fromField i v = fromIntegralField "Int" intDecimalPrec 0 i v

instance FromField Int16 where
  fromField = fromIntegralField "Int16" 4 0

instance FromField Int32 where
  fromField = fromIntegralField "Int32" 9 0

instance FromField Int64 where
  fromField _ (NativeInt64 x) = pure x
  fromField i v = convError "Int64" i v

instance FromField Word where
  fromField i v = fromIntegralField "Word" wordDecimalPrec 0 i v

instance FromField Word16 where
  fromField = fromIntegralField "Word16" 4 0

instance FromField Word32 where
  fromField = fromIntegralField "Word32" 9 0

instance FromField Word64 where
  fromField _ (NativeUint64 x) = pure x
  fromField i v = convError "Word64" i v

instance FromField Integer where
  fromField _ (NativeInt64 x) = pure $ fromIntegral x
  fromField _ (NativeUint64 x) = pure $ fromIntegral x
  fromField i v = convError "Integer" i v

instance FromField Float where
  fromField _ (NativeFloat x) = pure x
  fromField i v = convError "Float" i v

instance FromField Double where
  fromField _ (NativeDouble x) = pure x
  fromField _ (NativeFloat x) = pure $ realToFrac x
  fromField i v = convError "Double" i v

-- | Scientific is fetched as bytes which is the only way to preserve full precision of
-- Oracle NUMBER type. Beware of using it haphazardly as this incures a performance
-- penalty. Always use Double unless you really need additional precision.
instance FromField Scientific where
  fromField _ (NativeBytes x) = pure $ read $ B8.unpack x
  fromField i v = convError "Scientific" i v
  nativeTypeFor _ = Just NativeTypeBytes

instance FromField LocalTime where
  fromField _ (NativeTimestamp x) = do
    let d = fromGregorian (fromIntegral $ timestamp_year x)
                          (fromIntegral $ timestamp_month x)
                          (fromIntegral $ timestamp_day x)
        sec = fromIntegral $ timestamp_second x :: Pico
        fsec = MkFixed $ (fromIntegral $ timestamp_fsecond x) `div` 10
        t = TimeOfDay (fromIntegral $ timestamp_hour x)
                      (fromIntegral $ timestamp_minute x)
                      (sec + fsec)
    pure $ LocalTime d t
  fromField i v = convError "LocalTime" i v

instance FromField a => FromField (Maybe a) where
  fromField _ (NativeNull _) = pure Nothing
  fromField i v = Just <$> fromField i v
  nativeTypeFor _ = nativeTypeFor (Proxy :: Proxy a)

-- | A wrapper to provide truncating instances for number types that
-- override ODPI-C defaults.
newtype Exactly a = Exactly { unExactly :: a } deriving (Eq, Ord, Show)
deriving instance Num a => Num (Exactly a)
deriving instance Enum a => Enum (Exactly a)
deriving instance Real a => Real (Exactly a)
deriving instance Integral a => Integral (Exactly a)

instance FromField (Exactly Int) where
  fromField _ (NativeInt64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Int" i v
  nativeTypeFor _ = Just NativeTypeInt64

instance FromField (Exactly Int16) where
  fromField _ (NativeInt64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Int16" i v
  nativeTypeFor _ = Just NativeTypeInt64

instance FromField (Exactly Int32) where
  fromField _ (NativeInt64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Int32" i v
  nativeTypeFor _ = Just NativeTypeInt64

instance FromField (Exactly Word) where
  fromField _ (NativeUint64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Word" i v
  nativeTypeFor _ = Just NativeTypeUint64

instance FromField (Exactly Word16) where
  fromField _ (NativeUint64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Word16" i v
  nativeTypeFor _ = Just NativeTypeUint64

instance FromField (Exactly Word32) where
  fromField _ (NativeUint64 x) = pure $ Exactly $ fromIntegral x
  fromField i v = convError "Exactly Word32" i v
  nativeTypeFor _ = Just NativeTypeUint64

