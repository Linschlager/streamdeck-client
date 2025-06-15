module Github.Types where

import Prelude
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromJust)

data PrReviewState
    = Commented
    | ChangesRequested
    | Approved
    | Pending
    deriving stock (Show)

instance FromJSON PrReviewState where
    parseJSON =
        withText "PullRequestState" $ \case
            "COMMENTED" -> pure Commented
            "CHANGES_REQUESTED" -> pure ChangesRequested
            "APPROVED" -> pure Approved
            "PENDING" -> pure Pending
            _ -> mzero

data User = User
    { login :: String }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

data PrReview = PrReview
    { state :: PrReviewState
    , author :: User
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

data PullRequest = PullRequest
    { title :: String
    , number :: Int
    , url :: String
    , reviews :: [PrReview]
    }
    deriving stock (Show)

instance FromJSON PullRequest where
    parseJSON = withObject "PullRequest" $ \p -> do
        _ <- traceShowM ("This is the PR to parse: " ++ show p)
        reviews <- p .: "reviews"
        PullRequest
            <$> p .: "title"
            <*> p .: "number"
            <*> p .: "url"
            <*> reviews .: "nodes"

data RepositoryResponse = RepositoryResponse 
    { pullRequests :: [PullRequest] }
    deriving stock (Show)

instance FromJSON RepositoryResponse where
    parseJSON = withObject "RepositoryResponse" $ \o -> do
        flip (withObject "RepositoryData") (fromJust $ KeyMap.lookup "data" o) $ \i -> do
            flip (withObject "Repository") (fromJust $ KeyMap.lookup "repository" i) $ \r -> do
                prs <- r .: "pullRequests"
                RepositoryResponse <$> prs .: "nodes"
