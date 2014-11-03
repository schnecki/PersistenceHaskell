{-# LANGUAGE CPP #-}
-- DBObject.hs ---
--
-- Filename: DBObject.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug 20 10:27:24 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Nov  3 23:27:11 2014 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 1035
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


module Persistence.DBObject
    ( DBObject
      -- , DBObjectInternal
    , dbCols
    , dbColsDataTypes
    , dbColsPrimaryKey
    , dbCreateStmtAddCreateDefinition
    , dbData
    , dbSetData
    , createTable
    , saveObject
    , updateObject
    , loadObject
    , loadAllFromTable
    , getDbTableName
    , getIsDirty
    , getIsLoaded
    , newDBObject
    , setDirty
    , getDbIOConnection
    , getDbObject
    , updateDBObject
    ) where


import           Data.List             (intercalate)
import           Database.HDBC
import           Persistence.Constants
import           Persistence.DBResult
import           System.IO
#ifdef DEBUG
import           Control.Monad         (when)
import           Debug.Trace
#endif


-- | Create a database connection.
-- connect :: (DBObject a) => DBObjectInternal a -> IO Connection
-- connect obj = connection obj


-- | Internal data elements of objects
data DBObjectInternal conn obj = DBObjectInternal
    { isDirty     :: Bool
    , isLoaded    :: Bool
    , connection  :: IO conn
    , dbTableName :: String
    , dbObject    :: obj
    }

instance (Show b) => Show (DBObjectInternal a b) where
  show (DBObjectInternal d l _ n obj) = "DBObjectInternal { isDirty = " ++ show d
                                        ++ ", isLoaded = " ++ show l
                                        ++ ", dbTableName = " ++ n
                                        ++ ", dbObject = " ++ show obj ++ "}"


-- | Class for database objects. Database objects need to be an instance of
-- (implement) this class.
class DBObject a where
    dbCols                          :: a -> [String]
    dbColsDataTypes                 :: a -> [String]
    dbColsPrimaryKey                :: a -> [String]
    dbCreateStmtAddCreateDefinition :: a -> String
    dbData                          :: a -> [String]
    dbSetData                       :: a -> [SqlValue] -> a

-- | Create a new database object. First argument is the table name, second the
-- DBObject instance. It returns a DBObjectInternal
newDBObject :: (DBObject a, IConnection conn) => IO conn -> String -> a ->
               DBObjectInternal conn a
newDBObject = DBObjectInternal True False

updateDBObject :: (DBObject a, IConnection conn) => DBObjectInternal conn b -> a ->
                  DBObjectInternal conn a
updateDBObject old d = old { isDirty = True, dbObject = d }

-- | Get the dirty flag for the given object. It indicates if the setDirty
-- function was called. You should do this every-time the object changes!
getIsDirty :: (DBObject a, IConnection conn) => DBObjectInternal conn a -> Bool
getIsDirty = isDirty

getDbObject :: (DBObject a, IConnection conn) => DBObjectInternal conn a -> a
getDbObject = dbObject

-- | Get the loaded flag for the given object. It indicates if the object was
-- loaded from the database.
getIsLoaded :: (DBObject a, IConnection conn) => DBObjectInternal conn a -> Bool
getIsLoaded = isLoaded

-- | Get the database table name from the object.
getDbTableName :: (DBObject a, IConnection conn) => DBObjectInternal conn a -> String
getDbTableName = dbTableName

getDbIOConnection :: (DBObject a, IConnection conn) => DBObjectInternal conn a -> IO conn
getDbIOConnection = connection

-- Function to set the object dirty. The changes of dirty objects are being
-- saved to the database.
setDirty :: (DBObject a, IConnection conn) => DBObjectInternal conn a  -> DBObjectInternal conn a
setDirty obj = obj { isDirty = True }


-- | Function to set the object to be loaded from the database.
-- setLoaded :: (DBObject a) => DBObjectInternal a -> DBObjectInternal a
-- setLoaded obj = obj { isLoaded = True }


-- | Create a table with the information of the given object.
-- @DBObjectInternal a: Object to fetch column names and type definitions from.
createTable :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
               IO (DBResult (DBObjectInternal conn a))
createTable (DBObjectInternal _ _ c n obj) =
  doDmlStatement c sqlStmt
      where
        sqlStmt = "CREATE TABLE " ++ n ++ " (" ++ tblDef
                  ++ if null (dbCreateStmtAddCreateDefinition obj)
                     then ")"
                     else ", " ++ dbCreateStmtAddCreateDefinition obj ++ ")"
        tblDef = toStr $ zipWith (\a b -> a ++ " " ++ b) (dbCols obj) (dbColsDataTypes obj)


-- | Insert a new object to the database.
insertObject :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
                IO (DBResult (DBObjectInternal conn a))
insertObject (DBObjectInternal _ _ c n obj) =
  doDmlStatement c sqlStmt
      where
        sqlStmt = unwords ["INSERT INTO ", n
                          , "(", cols obj , ") VALUES ("
                          , rows obj, ");"]

-- | Insert new value or update row.
saveObject     :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
                  IO (DBResult (DBObjectInternal conn a))
saveObject obj
  | isDirty obj && isLoaded obj = updateObject obj
  | isDirty obj                = insertObject obj
  | otherwise                  = return $ DBSuccess [obj] -- already in DB


-- saveManyObjects     :: (DBObject a) => [DBObjectInternal a] -> IO (DBResult (DBObjectInternal a))
-- saveManyObjects obj =


-- Update an object in the database
updateObject     :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
                    IO (DBResult (DBObjectInternal conn a))
updateObject (DBObjectInternal _ _ c n obj) = doDmlStatement c sqlStmt
  where
    sqlStmt = unwords ["UPDATE ", n, " SET ",
                       columns, " WHERE ", pk ]
    columns = toStr $ zipWith (\a b -> a ++ " = " ++ b) (dbCols obj) (dbData obj)
    pk = unwords $ map (\(a, b) -> a ++ " = " ++  b) $
         filter (\(a, _) -> a `elem` dbColsPrimaryKey obj ) $
         zip (dbCols obj) (dbData obj)


loadObject :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
              IO (DBResult (DBObjectInternal conn a))
loadObject (DBObjectInternal _ _ c n obj) = do
  res <- doDrlStatement c sqlStmt
  case res of
    DBError txt -> return $ DBError txt
    DBSuccess r ->
        if null r
        then do
          -- hPutStrLn stderr $ "Error: Select statement returned no result: " ++ sqlStmt
          return $ DBError $ "Error: Select statement returned no result: " ++ sqlStmt
        else return $ DBSuccess [(DBObjectInternal False True c n . dbSetData obj) (r !! 0)]
    where
          sqlStmt = unwords ["SELECT ", columns, " FROM ", n
                            , " WHERE ", pk ]
          columns = toStr $ dbCols obj
          pk = unwords $ map (\(a, b) -> a ++ " = " ++  b) $
              filter (\(a, _) -> a `elem` dbColsPrimaryKey obj ) $
              zip (dbCols obj) (dbData obj)


-- | This function loads all objects from a table and returns it in a list. Of
-- course, the result is packed in the IO Monad.
loadAllFromTable     :: (DBObject a, IConnection conn) => DBObjectInternal conn a ->
                        IO (DBResult (DBObjectInternal conn a))
loadAllFromTable (DBObjectInternal _ _ c n obj) = do
  res <- doDrlStatement c sqlStmt
  case res of
    DBError txt -> return $ DBError txt
    DBSuccess r -> return $ DBSuccess $ map (DBObjectInternal False True c n . dbSetData obj) r

    where
      sqlStmt = unwords ["SELECT ", columns, " FROM ", n ]
      columns = toStr $ dbCols obj


-- | Helper function to execute a DRL SQL statement.
doDrlStatement :: (IConnection b) => IO b -> String -> IO (DBResult [SqlValue])
doDrlStatement c sqlStmt =
    handleSql (sqlException sqlStmt)
                  ( do
                    conn <- c
                    stmt <- prepare conn sqlStmt
                    execute stmt []
                    results <- fetchAllRows' stmt -- strictly fetch all rows
                    disconnect conn
                    print results
                    return $ dbResultSuccess results )


-- | Helper function to execute a DML SQL statement. This function opens a
-- connection, runs the given statement, commits and finally closes the
-- connection again. It returns the result from the run statement. In case a SQL
-- exception occurs, it will handle it, by printing out the error message.
doDmlStatement :: (IConnection conn) => IO conn -> String -> IO (DBResult a)
doDmlStatement c sqlStmt =
  handleSql (sqlException sqlStmt)
  (do
    conn <- c
    res <- -- handleSql (sqlException sqlStmt)
           (run conn sqlStmt [] >> return dbResultSuccessEmpty)
    commit conn
    disconnect conn
    return res)


-- | SQL Exception handler. In case a SqlException occurs this function can be
-- used to print out the error.
sqlException :: String -> SqlError -> IO (DBResult a)
sqlException sqlStmt (SqlError s n e) = do
  prefix <- dbPrefix
  -- hPutStrLn stderr $ "State: " ++ s
  hPutStrLn stderr $ prefix ++ "Native Error Code: " ++ show n
  hPutStrLn stderr $ prefix ++ "Error message: " ++ e
  hPutStrLn stderr $ prefix ++ "SQL Statement: " ++ sqlStmt
  appendFile dbLogFileName $ prefix ++ e ++ "\nSQL Statement: " ++ sqlStmt ++ "\n"
  return $ dbReturnQueryEmptySuccess n


-- | Prepare columns and create string for SQL statement.
cols     :: (DBObject a) => a -> String
cols obj = toStr $ map show $ dbCols obj

-- | Prepare row (data) and create string for SQL statement.
rows     :: (DBObject a) => a -> String
rows obj = toStr $ dbData obj

-- | Add a ", " in between of each String.
toStr :: [String] -> [Char]
toStr = intercalate ", "


--
-- DBObject.hs ends here
