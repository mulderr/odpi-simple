module Database.Odpi.Connection where

import Data.ByteString (ByteString)
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr

import Database.Odpi.LibDpi
import Database.Odpi.Types
import Database.Odpi.Util

connAddRef :: Connection -> IO Bool
connAddRef = fmap isOk . conn_addRef . getConn

connBeginDistribTrans :: Connection -> CLong -> ByteString -> ByteString -> IO Bool
connBeginDistribTrans p formatId transactionId branchId =
  pure (conn_beginDistribTrans (getConn p) formatId)
    >>= inStrLen transactionId
    >>= inStrLen branchId
    >>= fmap isOk

connBreakExecution :: Connection -> IO Bool
connBreakExecution = fmap isOk . conn_breakExecution . getConn

connChangePassword :: Connection -> ByteString -> ByteString -> ByteString -> IO Bool
connChangePassword p user oldpass newpass =
  pure (conn_changePassword $ getConn p)
    >>= inStrLen user
    >>= inStrLen oldpass
    >>= inStrLen newpass
    >>= fmap isOk

connClose :: Connection -> ConnCloseMode -> IO Bool
connClose p cm = isOk <$> conn_close (getConn p) (fromE cm) nullPtr 0

connCommit :: Connection -> IO Bool
connCommit = fmap isOk . conn_commit . getConn

connCreateEither
  :: Context
  -> ByteString -- username
  -> ByteString -- password
  -> ByteString -- connstring
  -- -> PtrCommonCreateParams
  -- -> PtrConnCreateParams
  -> IO (Either ErrorInfo Connection)
connCreateEither (Context ctx) user pass connstr =
  pure (conn_create ctx)
    >>= inStrLen user
    >>= inStrLen pass
    >>= inStrLen connstr
    >>= inPtr (\p -> context_initCommonCreateParams ctx p)
    >>= inPtr (\p -> context_initConnCreateParams ctx p)
    >>= out1Either ctx
    >>= pure . fmap (Connection . (,) ctx)

connCreate :: Context -> ByteString -> ByteString -> ByteString -> IO Connection
connCreate ctx user pass connstr = connCreateEither ctx user pass connstr >>= throwLeft

connGetCurrentSchema :: Connection -> IO ByteString
connGetCurrentSchema = runByteString conn_getCurrentSchema

connGetEdition :: Connection -> IO ByteString
connGetEdition = runByteString conn_getEdition

connGetEncodingInfo :: Connection -> IO EncodingInfo
connGetEncodingInfo = runStorable conn_getEncodingInfo

connGetExternalName :: Connection -> IO ByteString
connGetExternalName = runByteString conn_getExternalName

connGetInternalName :: Connection -> IO ByteString
connGetInternalName = runByteString conn_getInternalName

connGetLTXID :: Connection -> IO ByteString
connGetLTXID = runByteString conn_getLTXID

connGetObjectType :: Connection -> ByteString -> IO (Ptr DpiObjectType)
connGetObjectType c name =
  pure (conn_getObjectType (getConn c))
    >>= inStrLen name
    >>= out1 (getCtx c)

connGetServerVersion :: Connection -> IO (ByteString, VersionInfo)
connGetServerVersion c =
  pure (conn_getServerVersion (getConn c))
    >>= out3 (getCtx c)
    >>= \(p, l, v) -> (,) <$> packBsLen (p, l) <*> pure v

connGetStmtCacheSize :: Connection -> IO CUInt
connGetStmtCacheSize = runStorable conn_getStmtCacheSize

connPing :: Connection -> IO Bool
connPing c = isOk <$> conn_ping (getConn c)

-- TODO: handle tag
connPrepareStmt :: Connection -> Bool -> ByteString -> IO Statement
connPrepareStmt c scrollable sql =
  pure (conn_prepareStmt (getConn c) (fromBool scrollable))
    >>= inStrLen sql
    >>= inStrLenMaybe Nothing
    >>= out1 (getCtx c)
    >>= pure . Statement . (,) (getCtx c)

connRelease :: Connection -> IO Bool
connRelease c = isOk <$> conn_release (getConn c)

connRollback :: Connection -> IO Bool
connRollback c = isOk <$> conn_rollback (getConn c)

connSetAction :: Connection -> ByteString -> IO Bool
connSetAction = runSetString conn_setAction

connSetClientIdentifier :: Connection -> ByteString -> IO Bool
connSetClientIdentifier = runSetString conn_setClientIdentifier

connSetClientInfo :: Connection -> ByteString -> IO Bool
connSetClientInfo = runSetString conn_setClientInfo

connSetCurrentSchema :: Connection -> ByteString -> IO Bool
connSetCurrentSchema = runSetString conn_setClientInfo

connSetDbOp :: Connection -> ByteString -> IO Bool
connSetDbOp = runSetString conn_setDbOp

connSetExternalName :: Connection -> ByteString -> IO Bool
connSetExternalName = runSetString conn_setExternalName

connSetInternalName :: Connection -> ByteString -> IO Bool
connSetInternalName = runSetString conn_setInternalName

connSetModule :: Connection -> ByteString -> IO Bool
connSetModule = runSetString conn_setModule

connSetStmtCacheSize :: Connection -> Word32 -> IO Bool
connSetStmtCacheSize c cacheSize = isOk <$> conn_setStmtCacheSize (getConn c) (fromIntegral cacheSize)

connShutdownDatabase :: Connection -> ShutdownMode -> IO Bool
connShutdownDatabase c mode = isOk <$> conn_shutdownDatabase (getConn c) (fromE mode)

connStartupDatabase :: Connection -> StartupMode -> IO Bool
connStartupDatabase c mode = isOk <$> conn_startupDatabase (getConn c) (fromE mode)

connSubscribe :: Connection -> Ptr SubscrCreateParams -> IO (Ptr DpiSubscr)
connSubscribe c params = out1 (getCtx c) (conn_subscribe (getConn c) params)

connUnsubscribe :: Connection -> Ptr DpiSubscr -> IO Bool
connUnsubscribe c subscr = isOk <$> conn_unsubscribe (getConn c) subscr

