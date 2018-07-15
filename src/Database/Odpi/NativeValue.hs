{-# language
    DeriveGeneric
  , LambdaCase
#-}

module Database.Odpi.NativeValue where

import Data.ByteString (ByteString)
import Data.Hashable
import Data.Int
import Data.Word
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import GHC.Generics

import Database.Odpi.Data
import Database.Odpi.LibDpi

-- | See dpiDataBuffer, NativeNull added to avoid using Maybe and keep things flat
data NativeValue
  = NativeNull NativeTypeNum
  | NativeBool Bool
  | NativeInt64 Int64
  | NativeUint64 Word64
  | NativeFloat Float
  | NativeDouble Double
  | NativeBytes ByteString
  | NativeTimestamp Timestamp
  | NativeIntervalDs IntervalDs
  | NativeIntervalYm IntervalYm
  | NativeLob (Ptr DpiLob)
  | NativeObject (Ptr DpiObject)
  | NativeStmt (Ptr DpiStmt)
  | NativeRowid (Ptr DpiRowid)
  deriving (Eq, Generic, Show)

instance Hashable NativeValue

nativeValueToTypeNum :: NativeValue -> NativeTypeNum
nativeValueToTypeNum = \case
  NativeNull ty -> ty
  NativeBool _ -> NativeTypeBoolean
  NativeInt64 _ -> NativeTypeInt64
  NativeUint64 _ -> NativeTypeUint64
  NativeFloat _ -> NativeTypeFloat
  NativeDouble _ -> NativeTypeDouble
  NativeBytes _ -> NativeTypeBytes
  NativeTimestamp _ -> NativeTypeTimestamp
  NativeIntervalDs _ -> NativeTypeIntervalDs
  NativeIntervalYm _ -> NativeTypeIntervalYm
  NativeLob _ -> NativeTypeLob
  NativeObject _ -> NativeTypeObject
  NativeStmt _ -> NativeTypeStmt
  NativeRowid _ -> NativeTypeRowid

toNativeValue :: NativeTypeNum -> PtrData -> IO NativeValue
toNativeValue ty p = do
  isNull <- isDataNull p
  if isNull then pure (NativeNull ty) else go ty
  where
    go = \case
      NativeTypeBoolean -> NativeBool <$> getBool p
      NativeTypeInt64 -> NativeInt64 <$> getInt64 p
      NativeTypeUint64 -> NativeUint64 <$> getUint64 p
      NativeTypeFloat -> NativeFloat <$> getFloat p
      NativeTypeDouble -> NativeDouble <$> getDouble p
      NativeTypeBytes -> NativeBytes <$> getBytes p
      NativeTypeTimestamp -> NativeTimestamp <$> getTimestamp p
      NativeTypeIntervalDs -> NativeIntervalDs <$> getIntervalDs p
      NativeTypeIntervalYm -> NativeIntervalYm <$> getIntervalYm p
      NativeTypeLob -> NativeLob <$> getLob p
      NativeTypeObject -> NativeObject <$> getObject p
      NativeTypeStmt -> NativeStmt <$> getStmt p
      NativeTypeRowid -> error "not implemented"

withNativeValue :: NativeValue -> (NativeTypeNum -> PtrData -> IO a) -> IO a
withNativeValue v f =
  alloca $ \p -> do
    let ty = nativeValueToTypeNum v
    case v of
      NativeNull _ -> pure ()
      NativeBool x -> setBool p x
      NativeInt64 x -> setInt64 p x
      NativeUint64 x -> setUint64 p x
      NativeFloat x -> setFloat p x
      NativeDouble x -> setDouble p x
      NativeBytes x -> setBytes p x
      NativeTimestamp x -> setTimestamp p x
      NativeIntervalDs x -> setIntervalDs p x
      NativeIntervalYm x -> setIntervalYm p x
      NativeLob x -> setLob p x
      NativeObject x -> setObject p x
      NativeStmt x -> setStmt p x
      NativeRowid _ -> error "not implemented"
    f ty p

isNativeNull :: NativeValue -> Bool
isNativeNull (NativeNull _) = True
isNativeNull _ = False
