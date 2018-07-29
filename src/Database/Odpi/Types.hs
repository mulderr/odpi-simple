module Database.Odpi.Types where

import Control.Exception (Exception)
import Foreign.Ptr (Ptr)
import Database.Odpi.LibDpi

newtype Context = Context { unContext :: Ptr DpiContext } deriving Show

newtype Connection = Connection { unConnection :: (Ptr DpiContext, Ptr DpiConn) } deriving Show

getConn :: Connection -> Ptr DpiConn
getConn = snd . unConnection

newtype Pool = Pool { unPool :: (Ptr DpiContext, Ptr DpiPool) } deriving Show

getPool :: Pool -> Ptr DpiPool
getPool = snd . unPool

newtype Statement = Statement { unStatement :: (Ptr DpiContext, Ptr DpiStmt) } deriving Show

getStmt :: Statement -> Ptr DpiStmt
getStmt = snd . unStatement

newtype Var = Var { unVar :: (Ptr DpiContext, Ptr DpiVar) } deriving Show

getVar :: Var -> Ptr DpiVar
getVar = snd . unVar

class HasContext a where
  getCtx :: a -> Ptr DpiContext

instance HasContext Context where
  getCtx = unContext

instance HasContext Connection where
  getCtx = fst . unConnection

instance HasContext Pool where
  getCtx = fst . unPool

instance HasContext Statement where
  getCtx = fst . unStatement

instance HasContext Var where
  getCtx = fst . unVar

data DpiException
  = DpiException ErrorInfo
  deriving Show

instance Exception DpiException


