module Database.Odpi.Util where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.ByteString.Unsafe as B
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Database.Odpi.LibDpi
import Database.Odpi.Types

isOk :: CInt -> Bool
isOk = (== dpiSuccess)

-- | Only use with functions taking `const char *`, otherwise unsafe
inStr :: ByteString -> (Ptr CChar -> r) -> IO r
inStr b f = B.unsafeUseAsCString b $ \s -> pure $ f s

-- | Only use with functions taking `const char *`, otherwise unsafe
inStrLen :: ByteString -> (Ptr CChar -> CUInt -> r) -> IO r
inStrLen b f = B.unsafeUseAsCStringLen b $ \(s, n) -> pure $ f s (fromIntegral n)

-- | Only use with functions taking `const char *`, otherwise unsafe
inStrLenMaybe :: Maybe ByteString -> (Ptr CChar -> CUInt -> r) -> IO r
inStrLenMaybe mb f =
  case mb of
    Just b -> inStrLen b f
    Nothing -> pure $ f nullPtr 0

inPtr :: (Storable a) => (Ptr a -> IO b) -> (Ptr a -> r) -> IO r
inPtr initializePtr f = alloca $ \p -> initializePtr p >> pure (f p)

throwLeft :: Either ErrorInfo a -> IO a
throwLeft = either (throwIO . DpiException) pure

out1Either :: (Storable a) => Ptr DpiContext -> (Ptr a -> IO CInt) -> IO (Either ErrorInfo a)
out1Either ctx f =
  alloca $ \pa -> do
    r <- f pa
    if isOk r
      then fmap Right $ peek pa
      else fmap Left $ contextGetError ctx

out2Either :: (Storable a, Storable b) => Ptr DpiContext -> (Ptr a -> Ptr b -> IO CInt) -> IO (Either ErrorInfo (a, b))
out2Either ctx f =
  alloca $ \pa -> alloca $ \pb -> do
    r <- f pa pb
    if isOk r
      then fmap Right $ (,) <$> peek pa <*> peek pb
      else fmap Left $ contextGetError ctx

out3Either :: (Storable a, Storable b, Storable c) => Ptr DpiContext -> (Ptr a -> Ptr b -> Ptr c -> IO CInt) -> IO (Either ErrorInfo (a, b, c))
out3Either ctx f =
  alloca $ \pa -> alloca $ \pb -> alloca $ \pc -> do
    r <- f pa pb pc
    if isOk r
      then fmap Right $ (,,) <$> peek pa <*> peek pb <*> peek pc
      else fmap Left $ contextGetError ctx

out1 :: (Storable a) => Ptr DpiContext -> (Ptr a -> IO CInt) -> IO a
out1 ctx f = out1Either ctx f >>= throwLeft

out2 :: (Storable a, Storable b) => Ptr DpiContext -> (Ptr a -> Ptr b -> IO CInt) -> IO (a, b)
out2 ctx f = out2Either ctx f >>= throwLeft

out3 :: (Storable a, Storable b, Storable c) => Ptr DpiContext -> (Ptr a -> Ptr b -> Ptr c -> IO CInt) -> IO (a, b, c)
out3 ctx f = out3Either ctx f >>= throwLeft

runStorable :: (Storable a) => (Ptr DpiConn -> Ptr a -> IO CInt) -> Connection -> IO a
runStorable f c = out1 (getCtx c) (f (getConn c))

runStorableP :: (Storable a) => (Ptr DpiPool -> Ptr a -> IO CInt) -> Pool -> IO a
runStorableP f c = out1 (getCtx c) (f (getPool c))

runByteString :: (Ptr DpiConn -> Ptr (Ptr CChar) -> Ptr CUInt -> IO CInt) -> Connection -> IO ByteString
runByteString f c = out2 (getCtx c) (f (getConn c)) >>= packBsLen

packBsLen :: (Integral l) => (Ptr CChar, l) -> IO ByteString
packBsLen p = B.packCStringLen $ fmap fromIntegral p

runSetString :: (Ptr DpiConn -> Ptr CChar -> CUInt -> IO CInt) -> Connection -> ByteString -> IO Bool
runSetString f c s =
  pure (f (getConn c))
    >>= inStrLen s
    >>= fmap isOk

runSetIntegralP :: (Integral a, Integral b) => (Ptr DpiPool -> a -> IO CInt) -> Pool -> b -> IO Bool
runSetIntegralP f p x = isOk <$> f (getPool p) (fromIntegral x)

contextGetError :: Ptr DpiContext -> IO ErrorInfo
contextGetError ctx =
  alloca $ \p -> do
    context_getError ctx p
    peek p

