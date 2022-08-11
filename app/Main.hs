{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Control.Monad.Logger
import Database.Persist.Sqlite
import Control.Monad.Reader

runApp :: AppT ()
runApp = do
  lineItems <- runDB $ do
    runMigration migrateAll 
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    selectList @LineItem @SqlBackend @IO [] []
  liftIO $ print (lineItems)

main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT (runDB $ runMigration migrateAll) env 
  runAppT runApp env