{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  where

import           Control.Monad        (forM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteStringL (readFile, writeFile)
import           Data.List            (sortOn)
import           GHC.Generics         (Generic)
import           System.Directory
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.FilePath
import           System.IO
import           System.Process       (readProcess)
import           System.ReadEditor    (readEditor)
import           Text.RawString.QQ
import           Text.Read            (readMaybe)

printUsage :: Handle -> IO ()
printUsage h = do
    hPutStrLn h [r|
  Usage: git issues <command> [<args>]

  Commands:
    create   Create an issue
    edit     Edit an issue
    list     List issues
    show     Show an existing issue
    close    Close an issue
    reopen   Reopen an issue
    destroy  Delete an issue
    resolve  Print the path to the issues JSON file
|]

data IssueState = IssueStateOpen
                | IssueStateClosed
  deriving(Generic, Show)

instance ToJSON IssueState
instance FromJSON IssueState

data Issue = Issue { issueTitle  :: String
                   , issueBody   :: String
                   , issueNumber :: Int
                   , issueState  :: IssueState
                   }
  deriving(Generic, Show)

instance ToJSON Issue
instance FromJSON Issue

data Store = Store { storeLatestIssue :: Int
                   , storeIssues      :: [Issue]
                   }
  deriving(Generic, Show)

instance ToJSON Store
instance FromJSON Store

gitRepository :: IO String
gitRepository = init <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

readOrCreateStore :: String -> IO Store
readOrCreateStore repo = do
    storeExists <- doesFileExist (repo </> ".issues.json")
    if storeExists
        then readStore repo
        else createStore repo

readStore :: String -> IO Store
readStore repo = do
    mstore <- decode <$> ByteStringL.readFile (repo </> ".issues.json")
    case mstore of
        Just store -> return store
        Nothing -> error "Failed to parse .issues.json"

createStore :: String -> IO Store
createStore repo = do
    let store = Store { storeLatestIssue = 0
                      , storeIssues = []
                      }
    writeStore repo store
    return store

writeStore :: String -> Store -> IO ()
writeStore repo store =
    ByteStringL.writeFile (repo </> ".issues.json") $ encode store

createIssue :: [String] -> IO ()
createIssue _ = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    sissue <- readEditor
    let issueLines = lines sissue
        missue = case issueLines of
            title:_:lbody ->
                Just $ Issue { issueTitle = title
                             , issueBody = unlines lbody
                             , issueNumber = (storeLatestIssue store) + 1
                             , issueState = IssueStateOpen
                             }
            title:_ ->
                Just $ Issue { issueTitle = title
                             , issueBody = ""
                             , issueNumber = (storeLatestIssue store) + 1
                             , issueState = IssueStateOpen
                             }
            _ -> Nothing

    case missue of
        Just issue -> do
            writeStore repo store { storeIssues = issue : storeIssues store
                                  , storeLatestIssue = issueNumber issue
                                  }
            putStrLn $
                "Issue #" ++
                show (issueNumber issue) ++
                " \"" ++ issueTitle issue ++ "\"" ++
                " created"
        Nothing ->
            error "Failed to parse issue. Aborting."

editIssue :: [String] -> IO ()
editIssue = undefined

listIssues :: [String] -> IO ()
listIssues _ = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    forM_ (sortOn issueNumber (storeIssues store)) $ \issue -> do
        putStrLn $
            "#" ++
            (show (issueNumber issue)) ++ " " ++
            (issueTitle issue)

showIssue :: [String] -> IO ()
showIssue = undefined

destroyIssue :: [String] -> IO ()
destroyIssue = undefined

closeIssue :: [String] -> IO ()
closeIssue (query:_) = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    case readMaybe query of
        Just nissue -> do
            let helper issue
                    | issueNumber issue == nissue =
                          issue { issueState = IssueStateClosed }
                    | otherwise = issue
                storeIssues' = map helper (storeIssues store)
            writeStore repo $ store { storeIssues = storeIssues' }
            putStrLn $
                "Issue #" ++ show nissue ++ " closed"
        Nothing -> undefined
closeIssue _ = exitFailure

reopenIssue :: [String] -> IO ()
reopenIssue (query:_) = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    case readMaybe query of
        Just nissue -> do
            let helper issue
                    | issueNumber issue == nissue =
                          issue { issueState = IssueStateOpen }
                    | otherwise = issue
                storeIssues' = map helper (storeIssues store)
            writeStore repo $ store { storeIssues = storeIssues' }
            putStrLn $
                "Issue #" ++ show nissue ++ " opened"
        Nothing -> undefined
reopenIssue _ = exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        "create" : args' -> createIssue args'
        "edit" : args' -> editIssue args'
        "list" : args' -> listIssues args'
        "show" : args' -> showIssue args'
        "destroy" : args' -> destroyIssue args'
        "close" : args' -> closeIssue args'
        "reopen" : args' -> reopenIssue args'
        "resolve" : _ -> do
            repo <- gitRepository
            putStrLn $ repo </> ".issues.json"
        _ -> printUsage stderr >> exitFailure
