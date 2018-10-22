{-# language
    ScopedTypeVariables
#-}

module Database.Odpi
  ( module Database.Odpi
  , module Database.Odpi.Context
  , module Database.Odpi.Connection
  , module Database.Odpi.FromField
  , module Database.Odpi.FromRow
  , module Database.Odpi.NativeValue
  , module Database.Odpi.Pool
  , module Database.Odpi.Statement
  , module Database.Odpi.Types

  -- * Enumerations
  , Lib.AuthMode (..)
  , Lib.ConnCloseMode (..)
  , Lib.CreateMode (..)
  , Lib.DeqMode (..)
  , Lib.DeqNavigation (..)
  , Lib.EventType (..)
  , Lib.ExecMode (..)
  , Lib.FetchMode (..)
  , Lib.MessageDeliveryMode (..)
  , Lib.MessageState (..)
  , Lib.NativeTypeNum (..)
  , Lib.OpCode (..)
  , Lib.OracleTypeNum (..)
  , Lib.PoolCloseMode (..)
  , Lib.PoolGetMode (..)
  , Lib.Purity (..)
  , Lib.ShutdownMode (..)
  , Lib.StartupMode (..)
  , Lib.StatementType (..)
  -- , Lib.SubscrGroupingClass (..)
  -- , Lib.SubscrGroupingType (..)
  , Lib.SubscrNamespace (..)
  , Lib.SubscrProtocol (..)
  , Lib.SubscrQOS (..)
  , Lib.Visibility (..)

  -- * Public Structures
  , Lib.QueryInfo
  , Lib.IntervalDs
  , Lib.IntervalYm
  , Lib.Timestamp

  , Lib.PtrData
  , isDataNull

  , OdpiConf (..)

  -- * Re-exports
  , Only (..)
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Tuple.Only

import qualified Database.Odpi.LibDpi as Lib
import Database.Odpi.Context
import Database.Odpi.Connection
import Database.Odpi.Data
import Database.Odpi.FromField
import Database.Odpi.FromRow
import Database.Odpi.NativeValue
import Database.Odpi.Pool
import Database.Odpi.Statement
import Database.Odpi.Types

data OdpiConf
  = OdpiConf
  { _odpiConf_username :: ByteString
  , _odpiConf_password :: ByteString
  , _odpiConf_connstr :: ByteString
  } deriving (Eq, Show)

withContext :: (Context -> IO a) -> IO a
withContext = bracket contextCreate contextDestroy

withConnection :: Context -> OdpiConf -> (Connection -> IO a) -> IO a
withConnection ctx c =
  withConnection' ctx (_odpiConf_username c) (_odpiConf_password c) (_odpiConf_connstr c)

withConnection' :: Context -> ByteString -> ByteString -> ByteString -> (Connection -> IO a) -> IO a
withConnection' ctx uname pass connstr =
  bracket (connCreate ctx uname pass connstr)
          (flip connClose Lib.ModeConnCloseDefault)

withPool :: Context -> OdpiConf -> (Pool -> IO a) -> IO a
withPool ctx c =
  withPool' ctx (_odpiConf_username c) (_odpiConf_password c) (_odpiConf_connstr c)

withPool' :: Context -> ByteString -> ByteString -> ByteString -> (Pool -> IO a) -> IO a
withPool' ctx uname pass connstr =
  bracket (poolCreate ctx uname pass connstr)
          (flip poolClose Lib.ModePoolCloseDefault)

withStatement :: Connection -> Bool -> ByteString -> (Statement -> IO a) -> IO a
withStatement c scrollable sql =
  bracket (connPrepareStmt c scrollable sql)
          (stmtRelease)

-- | Naive query runner returning list
querySimple :: forall a. (FromRow a) => Connection -> ByteString -> IO [a]
querySimple conn sql =
  withStatement conn False sql $ \s -> do
    _ <- stmtExecute s Lib.ModeExecDefault
    defineValuesForRow (Proxy :: Proxy a) s
    go s
  where
    go s = do
      midx <- stmtFetch s
      case midx of
        Just _ -> do
          x <- fromRow s
          go s >>= pure . (x :)
        Nothing -> pure []
