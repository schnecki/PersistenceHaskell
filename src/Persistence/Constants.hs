-- Constants.hs ---
--
-- Filename: Constants.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug 20 11:38:49 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Aug 26 20:45:00 2014 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 34
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--
--
--

-- Code:


module Persistence.Constants where

import           Data.Time
import           Database.HDBC
import           Database.HDBC.Sqlite3

dbFileName :: String
dbFileName = "database.sql"


dbLogFileName :: String
dbLogFileName = "log_db.log"

dbPrefix :: IO String
dbPrefix = do
  t <- fmap show getCurrentTime
  return $ t ++ ": "

-- | Create a database connection.
dbConnection :: IO Connection
dbConnection = connectSqlite3 dbFileName

--
-- Constants.hs ends here
