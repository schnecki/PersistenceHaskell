{-# LANGUAGE CPP #-}
-- TestObject.hs ---
--
-- Filename: TestObject.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug 20 10:28:18 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Aug 26 20:50:12 2014 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 264
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
--

-- Code:


module Persistence.TestObject
    ( nr, name
    , DBTestObject
    ) where

import           Database.HDBC
import           Persistence.Constants
import           Persistence.DBObject
import           Persistence.DBResult
import           System.Posix.Unistd

#ifdef DEBUG
-- import           Debug.Trace
#endif


data DBTestObject = Test
                  { nr   :: Int
                  , name :: String
                  } deriving (Show)

dbTableName :: String
dbTableName = "testobjecttable2"


instance DBObject DBTestObject where
    dbCols _ = ["nr", "name"]
    dbColsDataTypes _ = ["INTEGER NOT NULL", "VARCHAR(30)"]
    dbColsPrimaryKey _ = ["nr"]
    dbData (Test n a) = [show n, show a]
    dbSetData obj d = obj { nr = fromSql $ d !! 0
                          , name = fromSql $ d !! 1}
    dbCreateStmtAddCreateDefinition _ = "PRIMARY KEY (nr)"

saveDBTestObject :: IO ()
saveDBTestObject = do
  let element = Test 4344 "rainer"
      dbObj = newDBObject dbConnection dbTableName element
  -- dbRes <- saveObject dbObj
  -- case dbRes of
  --   DBError txt -> putStrLn $ "ERROR saving object:" ++ show txt
  --   DBSuccess res ->
  --       do
          -- putStrLn $ "SUCCESS:" ++ show res

          -- load object back
  loaded <- loadObject dbObj

  case loaded of
            DBError txt -> putStrLn $ "Error loading object:"  ++ show txt
            DBSuccess [] -> putStrLn $ "No elementent in database."
            DBSuccess res2 ->
                do
                  putStrLn $ "Success: " ++ show res2
                  putStr "Updated: "
                  putStrLn $ show $ updateDBObject (head res2) test
                  -- usleep 1000000

                  saveObject $ updateDBObject (head res2) test
                  putStrLn $ "saved: " ++ show test

                      where
                        test :: DBTestObject
                        test = element { name = "Manuello" }


--
-- TestObject.hs ends here


