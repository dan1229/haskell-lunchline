module Main where

import Lib
import Control.Monad.Logger

runApp :: AppT ()
runApp = do
  lineItems <- runDB $ do
    runMigration migrateAll
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    selectList [] []
  liftIO $ print (lineItems :: [Entity LineItem])

main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT runApp env