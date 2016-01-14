{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main
  where

import           Control.Monad      (forM_, unless, when)
import           Data.List
import qualified Data.Map           as Map
import           Data.Maybe
import           Github.Issues      (issuesForRepo)
import qualified Github.Issues      as Github
import           GitIssues.Types
import           GitIssues.Web
import qualified Network.Wai        as Wai
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.FilePath
import           System.IO
import           System.Process
import           System.ReadEditor
import           Text.RawString.QQ
import           Text.Read          (readMaybe)
import           Web.Spock

printUsage :: Handle -> IO ()
printUsage h = hPutStrLn h [r|
  Usage: git issues <command> [<args>]

  Commands:
    create   Create an issue
    edit     Edit an issue
    list     List issues
    show     Show an existing issue
    sync     Sync issues with GitHub
    close    Close an issue
    reopen   Reopen an issue
    destroy  Delete an issue
    resolve  Print the path to the issues JSON file

    server   Start the git-issues UI
    serve
|]

gitRepository :: IO String
gitRepository = init <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

createIssue :: [String] -> IO ()
createIssue _ = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    sissue <- readEditor
    let issueLines = lines sissue
        missue = case issueLines of
            title:_:lbody ->
                Just Issue { issueTitle = title
                           , issueBody = unlines lbody
                           , issueNumber = storeLatestIssue store + 1
                           , issueState = IssueStateOpen
                           }
            title:_ ->
                Just Issue { issueTitle = title
                           , issueBody = ""
                           , issueNumber = storeLatestIssue store + 1
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
editIssue qs = case qs of
    (query:_) -> do
        repo <- gitRepository
        store <- readOrCreateStore repo
        case readMaybe query :: Maybe Int of
            Just nissue -> do
                let missue = find ((== nissue) . issueNumber) (storeIssues store)
                case missue of
                    Just issue -> editIssue' repo store issue
                    Nothing -> do
                        hPutStrLn stderr "Issue not found"
                        exitFailure
            Nothing -> editLastIssue repo store
    [] -> do
        repo <- gitRepository
        store <- readOrCreateStore repo
        editLastIssue repo store
  where
    editIssue' repo store issue = do
        let contents = issueTitle issue ++ "\n\n" ++ issueBody issue
        newContents <- readEditorWith contents
        let newIssue = case lines newContents of
                (title:_:bs) -> issue { issueTitle = title
                                        , issueBody = unlines bs
                                        }
                (title:_) -> issue { issueTitle = title
                                   , issueBody = ""
                                   }
                _ -> undefined
            store' = store { storeIssues =
                                 map (\i ->
                                       if issueNumber i == issueNumber newIssue
                                       then newIssue
                                       else i)
                                 (storeIssues store)
                           }
        writeStore repo store'
    editLastIssue repo store = do
        when (null (storeIssues store)) $ do
            hPutStrLn stderr "No issues in the store"
            exitFailure
        editIssue' repo store (last (storeIssues store))


listIssues :: [String] -> IO ()
listIssues _ = do
    repo <- gitRepository
    store <- readOrCreateStore repo
    forM_ (sortOn issueNumber (storeIssues store)) $ \issue ->
        putStrLn $
            "#" ++
            show (issueNumber issue) ++ " " ++
            issueTitle issue ++
            (if issueState issue == IssueStateClosed then " - closed" else "")

showIssue :: [String] -> IO ()
showIssue qs = case qs of
    (query:_) -> do
        repo <- gitRepository
        store <- readOrCreateStore repo
        case readMaybe query :: Maybe Int of
            Just nissue -> do
                let missue = find ((== nissue) . issueNumber) (storeIssues store)
                case missue of
                    Just issue -> showIssue' issue
                    Nothing -> do
                        hPutStrLn stderr "Issue not found"
                        exitFailure
            Nothing -> showLastIssue store
    [] -> showLastIssue =<< readOrCreateStore =<< gitRepository
  where
    showIssue' issue = do
        putStrLn $ issueTitle issue ++
            " (#" ++ show (issueNumber issue) ++ ")"
        unless (null (issueBody issue)) $ do
            putStrLn ""
            putStrLn $ issueBody issue
    showLastIssue store = if null (storeIssues store)
                          then do
                              hPutStrLn stderr "No issues in the store"
                              exitFailure
                          else showIssue' (last (storeIssues store))

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

syncIssues :: [String] -> IO ()
syncIssues _ = do
    repo <- gitRepository
    eissues <- gitRepositoryIssues
    case eissues of
        Left err -> error (show err)
        Right is -> do
            store <- readOrCreateStore repo
            putStrLn ("Found " ++ show (length is) ++ " to merge")
            let store' = store { storeIssues =
                                     mergeIssues (storeIssues store) (map githubIssueToGitIssuesIssue is)
                               }
            putStrLn ("Added " ++ show (length (storeIssues store') - length is) ++ " issues")
            writeStore repo store'
            return ()
  where
    mergeIssues = (++)
    -- indexBy fn xs = Map.fromList (map (\x -> (fn x, x)) xs)
    -- mergeIssues is1 is2 = map snd
    --     (Map.toList
    --      (Map.union (indexBy issueNumber is1) (indexBy issueNumber is2)))

githubIssueToGitIssuesIssue :: Github.Issue -> Issue
githubIssueToGitIssuesIssue i = Issue { issueTitle = Github.issueTitle i
                                      , issueBody = fromMaybe "" (Github.issueBody i)
                                      , issueNumber = Github.issueNumber i
                                      , issueState = if Github.issueState i == "open"
                                                     then IssueStateOpen
                                                     else IssueStateClosed
                                      }

gitRepositoryIssues :: IO (Either Github.Error [Github.Issue])
gitRepositoryIssues = githubRepository >>= \case
    Just (u, rp) ->
        issuesForRepo u rp []
    Nothing -> error "No github repository found"

githubRepository :: IO (Maybe (String, String))
githubRepository = do
    output <- map words . lines <$>
        readCreateProcess (shell "git remote -v") ""
    return $ extractUserRepo =<< find isGithubRepository output
  where
    extractUserRepo (_:url:_) = let rp = takeBaseName url
                                    u = reverse $ takeWhile
                                        (not . (\c -> c == ':' || c == '/'))
                                        (reverse (takeDirectory url))
                                in Just (u, rp)
    extractUserRepo _ = Nothing
    isGithubRepository :: [String] -> Bool
    isGithubRepository (_:url:_) = "github.com" `isInfixOf` url
    isGithubRepository _ = False

runGitIssuesServer :: [String] -> IO ()
runGitIssuesServer _ = do
    home <- getHomeDirectory
    repo <- gitRepository
    runSpock 3000 (spockT id (gitIssuesServer (home, repo)))

gitIssuesServerApp :: IO Wai.Application
gitIssuesServerApp = do
    home <- getHomeDirectory
    repo <- gitRepository
    spockAsApp (spockT id (gitIssuesServer (home, repo)))

main :: IO ()
main = do
    args <- getArgs
    case args of
        "create" : args' -> createIssue args'
        "edit" : args' -> editIssue args'
        "list" : args' -> listIssues args'
        "show" : args' -> showIssue args'
        "sync" : args' -> syncIssues args'
        "destroy" : args' -> destroyIssue args'
        "close" : args' -> closeIssue args'
        "reopen" : args' -> reopenIssue args'
        "resolve" : _ -> do
            repo <- gitRepository
            putStrLn $ repo </> ".issues.json"
        "server" : args' -> runGitIssuesServer args'
        "serve" : args' -> runGitIssuesServer args'
        _ -> printUsage stderr >> exitFailure
