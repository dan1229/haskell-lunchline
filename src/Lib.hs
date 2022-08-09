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

newtype SqlPersistT m a = SqlPersistT { unSqlPersistT :: ReaderT SqlBackend m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

newtype DB a = DB { unDB :: AppT a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)


-- rewrite runDB in terms of SqlPersistT

-- ORIGINAL
runDBOrig :: ReaderT SqlBackend IO a -> AppT a
runDBOrig body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool

-- hows this!?!?!?!?!?
runDB :: SqlPersistT m a -> DB a
runDB body = do
    pool <- asks envPool
    liftIO $ runSqlPool (unSqlPersistT body) pool