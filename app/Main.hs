{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Control.Monad.Logger
import Database.Persist.Sqlite
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental

-- runApp :: AppT ()
-- runApp = do
--   lineItems <- runDB $ do
--     runMigration migrateAll 
--     insert_ $ LineItem "Pizza" 11
--     insert_ $ LineItem "Burger" 12
--     selectList @LineItem @SqlBackend @IO [] []
--   liftIO $ print (lineItems)

getLineItemTotal :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
getLineItemTotal = selectSum $ do
  items <- from $ table @LineItem
  pure $ sum_ $ items ^. LineItemAmount
 where
  selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne
  -- Note: in Haskell >= 9, due to simplified subsumption, you will need to do replace selectOne with (\q -> selectOne q)

runApp :: AppT ()
runApp = do
  total <- runDB $ do
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    getLineItemTotal
  let remainingBudget = weeklyBudget - total
  liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget



main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT (runDB $ runMigration migrateAll) env 
  runAppT runApp env

