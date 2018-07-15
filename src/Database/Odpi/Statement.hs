module Database.Odpi.Statement where

import Data.ByteString (ByteString)
import Data.Word
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (toBool, fromBool)

import Database.Odpi.NativeValue
import Database.Odpi.LibDpi
import Database.Odpi.Types
import Database.Odpi.Util

stmtAddRef :: Statement -> IO Bool
stmtAddRef s = isOk <$> stmt_addRef (getStmt s)

stmtBindByName :: Statement -> ByteString -> Var -> IO Bool
stmtBindByName s name v =
  pure (stmt_bindByName (getStmt s))
    >>= inStrLen name
    >>= fmap isOk . ($ getVar v)

stmtBindByPos :: Statement -> Word32 -> Var -> IO Bool
stmtBindByPos s pos v = isOk <$> stmt_bindByPos (getStmt s) (fromIntegral pos) (getVar v)

stmtBindValueByName :: Statement -> ByteString -> NativeTypeNum -> Ptr Data -> IO Bool
stmtBindValueByName s name ty p =
  pure (stmt_bindValueByName (getStmt s))
    >>= inStrLen name
    >>= \f -> isOk <$> f (fromE ty) p

stmtBindValueByPos :: Statement -> Word32 -> NativeValue -> IO Bool
stmtBindValueByPos s pos v = withNativeValue v $ stmtBindValueByPos' s pos

stmtBindValueByPos' :: Statement -> Word32 -> NativeTypeNum -> Ptr Data -> IO Bool
stmtBindValueByPos' s pos ty p = isOk <$> stmt_bindValueByPos (getStmt s) (fromIntegral pos) (fromE ty) p

stmtClose :: Statement -> Maybe ByteString -> IO Bool
stmtClose s mtag =
  pure (stmt_close (getStmt s))
    >>= inStrLenMaybe mtag
    >>= fmap isOk

stmtDefine :: Statement -> Word32 -> Var -> IO Bool
stmtDefine s pos v = isOk <$> stmt_define (getStmt s) (fromIntegral pos) (getVar v)

stmtDefineValue :: Statement -> Word32 -> OracleTypeNum -> NativeTypeNum -> Word32 -> Bool -> Ptr DpiObjectType -> IO Bool
stmtDefineValue s pos oraTy ty size sizeIsBytes objType =
  isOk <$> stmt_defineValue (getStmt s) (fromIntegral pos) (fromE oraTy) (fromE ty) (fromIntegral size) (fromBool sizeIsBytes) objType

stmtExecute :: Statement -> ExecMode -> IO Word32
stmtExecute s mode = fromIntegral <$> out1 (getCtx s) (stmt_execute (getStmt s) (fromE mode))

stmtExecuteMany :: Statement -> ExecMode -> Word32 -> IO Bool
stmtExecuteMany s mode numIters = isOk <$> stmt_executeMany (getStmt s) (fromE mode) (fromIntegral numIters)

stmtFetch :: Statement -> IO (Maybe Word32)
stmtFetch s = do
  (found, bufferRowIndex) <- out2 (getCtx s) (stmt_fetch (getStmt s))
  pure $ if toBool found
           then Just $ fromIntegral bufferRowIndex
           else Nothing

-- stmtFetchRows
-- stmtBatchErrorCount
-- stmtBatchErrors
-- stmtGetBindCount
-- stmtGetBindNames
-- stmtGetFetchArraySize
-- stmtGetImplicitResult
-- stmtGetInfo

stmtGetNumQueryColumns :: Statement -> IO Word32
stmtGetNumQueryColumns s = fromIntegral <$> out1 (getCtx s) (stmt_getNumQueryColumns (getStmt s))

stmtGetQueryInfo :: Statement -> Word32 -> IO QueryInfo
stmtGetQueryInfo s pos = out1 (getCtx s) (stmt_getQueryInfo (getStmt s) (fromIntegral pos))

stmtGetQueryValue :: Statement -> Word32 -> IO NativeValue
stmtGetQueryValue s pos = stmtGetQueryValue' s pos >>= uncurry toNativeValue

stmtGetQueryValue' :: Statement -> Word32 -> IO (NativeTypeNum, PtrData)
stmtGetQueryValue' s pos = do
  (ty, p) <- out2 (getCtx s) (stmt_getQueryValue (getStmt s) (fromIntegral pos))
  pure (toE ty, p)

-- stmtGetRowCount
-- stmtGetRowCounts
-- stmtGetSubscrQueryId

stmtRelease :: Statement -> IO Bool
stmtRelease s = isOk <$> stmt_release (getStmt s)

-- stmtScroll
-- stmtSetFetchArraySize
