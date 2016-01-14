{-# LANGUAGE DeriveGeneric #-}
module GitIssues.Types
  where

import           Data.Aeson               (FromJSON, ToJSON, decode, encode)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as ByteStringL
import           GHC.Generics             (Generic)
import           System.Directory
import           System.FilePath

data IssueState = IssueStateOpen
                | IssueStateClosed
  deriving(Eq, Generic, Show)

instance ToJSON IssueState
instance FromJSON IssueState

-- data IssueProvider = IssueProviderGithub
--   deriving(Generic, Show)

-- instance ToJSON IssueProvider
-- instance FromJSON IssueProvider

-- type IssueId = Int

data Issue = Issue { issueTitle  :: String
                   , issueBody   :: String
                   , issueNumber :: Int
                   , issueState  :: IssueState
                   -- , issueOrigin :: (IssueProvider, IssueId)
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
    ByteStringL.writeFile (repo </> ".issues.json") $
    encodePretty' defConfig { confIndent = 2 } store
