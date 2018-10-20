{-# language
    ExplicitForAll
  , ExistentialQuantification
  , FlexibleContexts
  , ScopedTypeVariables
  , TypeApplications
#-}

module Database.Odpi.FromRow where

import Control.Exception (throwIO)
import Data.Proxy
import Data.Tuple.Only
import Data.Word

import Database.Odpi.FromField
import Database.Odpi.LibDpi
import Database.Odpi.Statement
import Database.Odpi.Types

data AnyField = forall a. FromField a => AF (Proxy a)

-- | A collection that can be converted from a sequence of fields
class FromRow a where
  -- | A list of target Haskell types for each column in a row
  --
  -- This is used to adjust how a value is fetched from the database if
  -- needed. For example Scientific would be fetched as NativeTypeDouble
  -- by default whereas we want NativeTypeBytes to get full precision.
  -- Uses nativeTypeFor from FromField.
  rowRep :: Proxy a -> [AnyField]

  -- | Fetch values from the current row
  fromRow :: Statement -> IO a

field :: FromField a => Statement -> Word32 -> IO a
field s i = do
  qi <- stmtGetQueryInfo s i
  v <- stmtGetQueryValue s i
  r <- fromField qi v
  case r of
    Ok a -> pure a
    Errors (e:_) -> throwIO e
    Errors _ -> error "empty exception list for Errors"

defineValuesForRow :: forall a. FromRow a => Proxy a -> Statement -> IO ()
defineValuesForRow p s = do
  n <- stmtGetNumQueryColumns s
  mapM_ f $ zip [1..n] (rowRep p)
  where
    f :: (Word32, AnyField) -> IO ()
    f (i, AF pa) =
      case nativeTypeFor pa of
        Nothing -> pure ()
        Just ty -> defineValueForTy s i ty

defineValueForTy :: Statement -> Word32 -> NativeTypeNum -> IO ()
defineValueForTy s i ty = do
  ti <- queryInfo_typeInfo <$> stmtGetQueryInfo s i
  _ <- stmtDefineValue s i (dataTypeInfo_oracleTypeNum ti) ty
                           (dataTypeInfo_dbSizeInBytes ti) True
                           (dataTypeInfo_objectType ti)
  pure ()

instance (FromField a) => FromRow (Only a) where
  rowRep _ = [AF (Proxy @a)]
  fromRow s = Only <$> field s 1

instance (FromField a, FromField b) => FromRow (a, b) where
  rowRep _ = [AF (Proxy @a), AF (Proxy @b)]
  fromRow s = (,) <$> field s 1 <*> field s 2

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
  rowRep _ = [AF (Proxy @a), AF (Proxy @b), AF (Proxy @c)]
  fromRow s = (,,) <$> field s 1 <*> field s 2 <*> field s 3

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
  rowRep _ = [AF (Proxy @a), AF (Proxy @b), AF (Proxy @c), AF (Proxy @d)]
  fromRow s = (,,,) <$> field s 1 <*> field s 2 <*> field s 3 <*> field s 4

instance ( FromField a, FromField b, FromField c, FromField d
         , FromField e ) => FromRow (a, b, c, d, e) where
  rowRep _ = [ AF (Proxy @a), AF (Proxy @b), AF (Proxy @c), AF (Proxy @d)
             , AF (Proxy @e)
             ]
  fromRow s = (,,,,) <$> field s 1 <*> field s 2 <*> field s 3 <*> field s 4
                     <*> field s 5

instance ( FromField a, FromField b, FromField c, FromField d
         , FromField e, FromField f ) => FromRow (a, b, c, d, e, f) where
  rowRep _ = [ AF (Proxy @a), AF (Proxy @b), AF (Proxy @c), AF (Proxy @d)
             , AF (Proxy @e), AF (Proxy @f)
             ]
  fromRow s = (,,,,,) <$> field s 1 <*> field s 2 <*> field s 3 <*> field s 4
                      <*> field s 5 <*> field s 6

instance ( FromField a, FromField b, FromField c, FromField d
         , FromField e, FromField f, FromField g ) => FromRow (a, b, c, d, e, f, g) where
  rowRep _ = [ AF (Proxy @a), AF (Proxy @b), AF (Proxy @c), AF (Proxy @d)
             , AF (Proxy @e), AF (Proxy @f), AF (Proxy @g)
             ]
  fromRow s = (,,,,,,) <$> field s 1 <*> field s 2 <*> field s 3 <*> field s 4
                       <*> field s 5 <*> field s 6 <*> field s 7

instance ( FromField a, FromField b, FromField c, FromField d
         , FromField e, FromField f, FromField g, FromField h ) => FromRow (a, b, c, d, e, f, g, h) where
  rowRep _ = [ AF (Proxy @a), AF (Proxy @b), AF (Proxy @c), AF (Proxy @d)
             , AF (Proxy @e), AF (Proxy @f), AF (Proxy @g), AF (Proxy @h)
             ]
  fromRow s = (,,,,,,,) <$> field s 1 <*> field s 2 <*> field s 3 <*> field s 4
                        <*> field s 5 <*> field s 6 <*> field s 7 <*> field s 8

