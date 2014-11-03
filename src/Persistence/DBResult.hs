-- DBResult.hs ---
--
-- Filename: DBResult.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug 20 10:26:43 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Fri Aug 22 15:28:27 2014 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 146
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


module Persistence.DBResult
    ( DBResult (DBSuccess, DBError)
    , dbReturnQueryEmptySuccess
    , dbErrorText
    , dbResult
    , dbResultSuccessEmpty
    , dbResultSuccess
    , fromDbResult
    , fstFromDbResult
    ) where


data DBResult a = DBError
    { dbErrorText ::  String
    } | DBSuccess {
      dbResult :: [a]
    } deriving (Show)


fromDbResult               :: DBResult a -> [a]
fromDbResult (DBSuccess x) = x
fromDbResult (DBError x)   = error x


fstFromDbResult               :: DBResult a -> a
fstFromDbResult (DBSuccess x) = head x
fstFromDbResult (DBError x)   = error x

dbResultSuccessEmpty :: DBResult a
dbResultSuccessEmpty = DBSuccess []


dbResultSuccess :: [a] -> DBResult a
dbResultSuccess a = DBSuccess a


dbReturnQueryEmptySuccess    :: Int -> DBResult a
dbReturnQueryEmptySuccess nr = case nr of
                                 0 -> DBSuccess []
                                 1 -> DBError "SQL error or missing database"
                                 2  -> DBError "Internal logic error in SQLite"
                                 3  -> DBError "Access permission denied"
                                 4  -> DBError "Callback routine requested an abort"
                                 5  -> DBError "The database file is locked"
                                 6  -> DBError "A table in the database is locked"
                                 7  -> DBError "A malloc() failed"
                                 8  -> DBError "Attempt to write a readonly database"
                                 9  -> DBError "Operation terminated by sqlite3_interrupt("
                                 10  -> DBError "Some kind of disk I/O error occurred"
                                 11  -> DBError "The database disk image is malformed"
                                 12  -> DBError "Unknown opcode in sqlite3_file_control()"
                                 13  -> DBError "Insertion failed because database is full"
                                 14  -> DBError "Unable to open the database file"
                                 15  -> DBError "Database lock protocol error"
                                 16  -> DBError "Database is empty"
                                 17  -> DBError "The database schema changed"
                                 18  -> DBError "String or BLOB exceeds size limit"
                                 19  -> DBError "Abort due to constraint violation"
                                 20  -> DBError "Data type mismatch"
                                 21  -> DBError "Library used incorrectly"
                                 22  -> DBError "Uses OS features not supported on host"
                                 23  -> DBError "Authorization denied"
                                 24  -> DBError "Auxiliary database format error"
                                 25  -> DBError "2nd parameter to sqlite3_bind out of range"
                                 26  -> DBError "File opened that is not a database file"
                                 27  -> DBError "Notifications from sqlite3_log()"
                                 28  -> DBError "Warnings from sqlite3_log()"
                                 100 -> DBError "sqlite3_step() has another row ready"
                                 101 -> DBError "sqlite3_step() has finished executing"


-- DBResult.hs ends here
