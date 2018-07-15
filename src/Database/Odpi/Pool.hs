module Database.Odpi.Pool where

import Data.ByteString (ByteString)
import Data.Word

import Database.Odpi.LibDpi
import Database.Odpi.Types
import Database.Odpi.Util

poolAcquireConnectionEither
  :: Pool
  -> Maybe ByteString
  -> Maybe ByteString
  -- -> ConnCreateParams
  -> IO (Either ErrorInfo Connection)
poolAcquireConnectionEither p muname mpass =
  let ctx = getCtx p
  in pure (pool_acquireConnection (getPool p))
    >>= inStrLenMaybe muname
    >>= inStrLenMaybe mpass
    >>= inPtr (context_initConnCreateParams ctx)
    >>= out1Either ctx
    >>= pure . fmap (Connection . (,) ctx)

poolAcquireConnection :: Pool -> Maybe ByteString -> Maybe ByteString -> IO Connection
poolAcquireConnection p muname mpass = poolAcquireConnectionEither p muname mpass >>= throwLeft

poolAddRef :: Pool -> IO Bool
poolAddRef p = isOk <$> pool_addRef (getPool p)

poolClose :: Pool -> PoolCloseMode -> IO Bool
poolClose p closeMode = isOk <$> pool_close (getPool p) (fromE closeMode)

poolCreateEither
  :: Context
  -> ByteString
  -> ByteString
  -> ByteString
  -- -> CommonCreateParams
  -- -> PoolCreateParams
  -> IO (Either ErrorInfo Pool)
poolCreateEither (Context ctx) uname pass connstr =
  pure (pool_create ctx)
    >>= inStrLen uname
    >>= inStrLen pass
    >>= inStrLen connstr
    >>= inPtr (\p -> context_initCommonCreateParams ctx p)
    >>= inPtr (\p -> context_initPoolCreateParams ctx p)
    >>= out1Either ctx
    >>= pure . fmap (Pool . (,) ctx)

poolCreate :: Context -> ByteString -> ByteString -> ByteString -> IO Pool
poolCreate ctx user pass connstr = poolCreateEither ctx user pass connstr >>= throwLeft

poolGetBusyCount :: Pool -> IO Word32
poolGetBusyCount = fmap fromIntegral . runStorableP pool_getBusyCount

poolGetEncodingInfo :: Pool -> IO EncodingInfo
poolGetEncodingInfo = runStorableP pool_getEncodingInfo

poolGetGetMode :: Pool -> IO PoolGetMode
poolGetGetMode = fmap toE . runStorableP pool_getGetMode

poolGetMaxLifetimeSession :: Pool -> IO Word32
poolGetMaxLifetimeSession = fmap fromIntegral . runStorableP pool_getMaxLifetimeSession

poolGetOpenCount :: Pool -> IO Word32
poolGetOpenCount = fmap fromIntegral . runStorableP pool_getOpenCount

poolGetStmtCacheSize :: Pool -> IO Word32
poolGetStmtCacheSize = fmap fromIntegral . runStorableP pool_getStmtCacheSize

poolGetTimeout :: Pool -> IO Word32
poolGetTimeout = fmap fromIntegral . runStorableP pool_getTimeout

poolGetWaitTimeout :: Pool -> IO Word32
poolGetWaitTimeout = fmap fromIntegral . runStorableP pool_getWaitTimeout

poolRelease :: Pool -> IO Bool
poolRelease p = isOk <$> pool_release (getPool p)

poolSetGetMode :: Pool -> PoolGetMode -> IO Bool
poolSetGetMode p mode = runSetIntegralP pool_setGetMode p (fromE mode :: Int)

poolSetMaxLifetimeSession :: Pool -> Word32 -> IO Bool
poolSetMaxLifetimeSession = runSetIntegralP pool_setMaxLifetimeSession

poolSetStmtCacheSize :: Pool -> Word32 -> IO Bool
poolSetStmtCacheSize = runSetIntegralP pool_setStmtCacheSize

poolSetTimeout :: Pool -> Word32 -> IO Bool
poolSetTimeout = runSetIntegralP pool_setTimeout

poolSetWaitTimeout :: Pool -> Word32 -> IO Bool
poolSetWaitTimeout = runSetIntegralP pool_setWaitTimeout

