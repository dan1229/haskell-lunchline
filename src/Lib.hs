module Lib
    ( someFunc
    ) where

import Data.Pool
import Database.Persist.Sqlite
import Control.Monad.Reader


data Env = Env { envPool :: Pool SqlBackend }


newtype AppT a = AppT { unAppT :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)
  
  
runAppT :: MonadIO m => AppT a -> Env -> m a
runAppT body env = liftIO $ runReaderT (unAppT body) env


runDB :: ReaderT SqlBackend IO a -> AppT a
runDB body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool