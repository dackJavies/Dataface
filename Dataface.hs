{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)

-- This is to give an example of how to connect to, read from, and write to a MongoDB instance
-- running locally. It requires the mongodb library, which I haven't figured out how to add
-- a package dependency for yet, and a running MongoDB instance.

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "dataface" run
    close pipe
    print e

run :: Action IO ()
run = do
    clearFaces
    insertFaces
    allFaces >>= printDocs "All Faces"

clearFaces :: Action IO ()
clearFaces = delete (select [] "face")

-- Example of how to insert multiple things at the same time. Tuples are stored as strings using
-- `show`, and can be converted back into tuples with proper integers with `read`.
insertFaces :: Action IO [Value]
insertFaces = insertMany "face" [
    ["name" =: "MyFace", "points" =: [show (0,0), show (0, 1), show (1, 1), show (1, 0)]],
    ["name" =: "YourFace", "points" =: [show (0,0), show (0, 1), show (1, 1), show (1, 0)]]]

-- Example of how to fetch many records, sorted by an attributes
allFaces :: Action IO [Document]
allFaces = rest =<< find (select [] "face") {sort = ["name" =: 1]}

-- Print attributes of all documents saved in the database, excluding their `_id`s.
printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs