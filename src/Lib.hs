module Lib where

import Data.Pool
import Database.Persist.Sqlite
import Control.Monad.Reader


data Env = Env { envPool :: Pool SqlBackend }


newtype AppT a = AppT { unAppT :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)
  
  
runAppT :: MonadIO m => AppT a -> Env -> m a
runAppT body env = liftIO $ runReaderT (unAppT body) env


-- TODO Try defining both SqlPersistT m a and DB a and try rewriting runDB in terms of them

type SqlPersistT = ReaderT SqlBackend

type DB a = forall m. MonadIO m => SqlPersistT m a

-- rewrite runDB in terms of SqlPersistT

-- ORIGINAL
runDB :: ReaderT SqlBackend IO a -> AppT a
runDB body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool

    
-- version 1
runDB1 :: MonadIO m => SqlPersistT m a -> AppT a
runDB1 body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool

    
-- version 2
runDB2 :: DB a -> AppT a
runDB2 body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool