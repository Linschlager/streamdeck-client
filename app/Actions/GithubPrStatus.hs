{-# LANGUAGE DuplicateRecordFields #-}
module Actions.GithubPrStatus where

import Prelude
import Data.ByteString.Char8 qualified as ByteString
import System.Environment ( getEnv )
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS (newTlsManager)

data GhRequest
    = ListPullRequests { repo :: String }
    | ListPullRequestReviews { repo :: String, prId :: Int }

data GithubUser = GithubUser
    { name :: String
    , avatarUrl :: String
    }
    deriving stock (Generic, Show)

instance FromJSON GithubUser where
    parseJSON = 
        withObject "GithubUser" $ \u -> GithubUser
            <$> (u .: "name" <|> u .: "login")
            <*> u .: "avatar_url"

data PullRequest = PullRequest
    { url :: String
    , title :: String
    , state :: String
    , id :: Int
    , number :: Int }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data PullRequestState
    = Commented
    | Approved
    | ChangesRequested
    deriving stock (Generic, Show, Eq)

instance FromJSON PullRequestState where
    parseJSON =
        withText "PullRequestState" $ \case
            "COMMENTED" -> pure Commented
            "APPROVED" -> pure Approved
            "CHANGES_REQUESTED" -> pure ChangesRequested
            _ -> mzero

data PullRequestReview = PullRequestReview
    { htmlUrl :: String
    , user :: GithubUser
    , state :: PullRequestState
    }
    deriving stock (Generic, Show)

instance FromJSON PullRequestReview where
    parseJSON =
        withObject "PullRequestReview" $ \d -> PullRequestReview
            <$> d .: "html_url"
            <*> d .: "user"
            <*> d .: "state"

authTokenFromEnv :: IO String
authTokenFromEnv = getEnv "GH_AUTHTOKEN"

-- | TODO: Make this configurable more easily / dynamic by user
repositoryFromEnv :: IO String
repositoryFromEnv = getEnv "GH_REPOSITORY"

baseUrl :: String -> String
baseUrl repo = "https://api.github.com/repos/" ++ repo

requestGithub :: GhRequest -> IO LazyByteString
requestGithub ghReq = do
    let url = case ghReq of
         (ListPullRequests {..}) -> baseUrl repo ++ "/pulls"
         (ListPullRequestReviews {..}) -> baseUrl repo ++ "/pulls/" ++ show prId ++ "/reviews"
    initial <- parseRequest url
    token <- authTokenFromEnv
    manager <- newTlsManager
    let request = initial { requestHeaders =
        [ (hAuthorization, ByteString.pack ("Bearer " ++ token) )
        , (hAccept, ByteString.pack "application/vnd.github+json")
        , (hUserAgent, ByteString.pack "rhine-streamdeck client")
        , ("X-GitHub-Api-Version", ByteString.pack "2022-11-28")
        ]}
    resp <- httpLbs request manager
    pure $ responseBody resp

pullRequestsForRepo :: IO [PullRequest]
pullRequestsForRepo = do
    repo <- repositoryFromEnv
    raw <- requestGithub (ListPullRequests {..})
    traceShowM raw
    either fail pure $ eitherDecode raw

pullRequestReviews :: Int -> IO [PullRequestReview]
pullRequestReviews prId = do
    repo <- repositoryFromEnv
    raw <- requestGithub (ListPullRequestReviews {..})
    either fail pure $ eitherDecode raw

