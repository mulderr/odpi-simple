module Database.Odpi.Data where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Storable

import Database.Odpi.LibDpi
import Database.Odpi.Util

isDataNull :: Ptr Data -> IO Bool
isDataNull = fmap data_isNull . peek

getBool :: Ptr Data -> IO Bool
getBool = fmap toBool . data_getBool

getBytes :: Ptr Data -> IO ByteString
getBytes p = data_getBytes p >>= peek >>= packBsLen . bytes_ptr

getDouble :: Ptr Data -> IO Double
getDouble = fmap (\(CDouble x) -> x) . data_getDouble

getFloat :: Ptr Data -> IO Float
getFloat = fmap (\(CFloat x) -> x) . data_getFloat

getInt64 :: Ptr Data -> IO Int64
getInt64 = fmap fromIntegral . data_getInt64

getIntervalDs :: Ptr Data -> IO IntervalDs
getIntervalDs p = data_getIntervalDS p >>= peek

getIntervalYm :: Ptr Data -> IO IntervalYm
getIntervalYm p = data_getIntervalYM p >>= peek

getLob :: Ptr Data -> IO (Ptr DpiLob)
getLob = data_getLOB

getObject :: Ptr Data -> IO (Ptr DpiObject)
getObject = data_getObject

getStmt :: Ptr Data -> IO (Ptr DpiStmt)
getStmt = data_getStmt

getTimestamp :: Ptr Data -> IO Timestamp
getTimestamp p = data_getTimestamp p >>= peek

getUint64 :: Ptr Data -> IO Word64
getUint64 = fmap fromIntegral . data_getUint64

setBool :: Ptr Data -> Bool -> IO ()
setBool p v = data_setBool p (fromBool v)

setBytes :: Ptr Data -> ByteString -> IO ()
setBytes p b = join $ inStrLen b (data_setBytes p)

setDouble :: Ptr Data -> Double -> IO ()
setDouble p x = data_setDouble p (CDouble x)

setFloat :: Ptr Data -> Float -> IO ()
setFloat p x = data_setFloat p (CFloat x)

setInt64 :: Ptr Data -> Int64 -> IO ()
setInt64 p x = data_setFloat p (fromIntegral x)

setIntervalDs :: Ptr Data -> IntervalDs -> IO ()
setIntervalDs p x =
  data_setIntervalDS p (fromIntegral $ intervalDs_days x)
                       (fromIntegral $ intervalDs_hours x)
                       (fromIntegral $ intervalDs_minutes x)
                       (fromIntegral $ intervalDs_seconds x)
                       (fromIntegral $ intervalDs_fseconds x)

setIntervalYm :: Ptr Data -> IntervalYm -> IO ()
setIntervalYm p x =
  data_setIntervalYM p (fromIntegral $ intervalYm_years x)
                       (fromIntegral $ intervalYm_months x)

setLob :: Ptr Data -> Ptr DpiLob -> IO ()
setLob = data_setLOB

setObject :: Ptr Data -> Ptr DpiObject -> IO ()
setObject = data_setObject

setStmt :: Ptr Data -> Ptr DpiStmt -> IO ()
setStmt = data_setStmt

setTimestamp :: Ptr Data -> Timestamp -> IO ()
setTimestamp p x =
  data_setTimestamp p (fromIntegral $ timestamp_year x)
                      (fromIntegral $ timestamp_month x)
                      (fromIntegral $ timestamp_day x)
                      (fromIntegral $ timestamp_hour x)
                      (fromIntegral $ timestamp_minute x)
                      (fromIntegral $ timestamp_second x)
                      (fromIntegral $ timestamp_fsecond x)
                      (fromIntegral $ timestamp_tzHourOffset x)
                      (fromIntegral $ timestamp_tzMinuteOffset x)

setUint64 :: Ptr Data -> Word64 -> IO ()
setUint64 p x = data_setUint64 p (fromIntegral x)

