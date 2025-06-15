module Github.GithubQueries where

import Prelude
import Data.Text qualified as Text
import Data.ByteString.Char8 qualified as ByteString
import System.Environment ( getEnv )
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS (newTlsManager)

authTokenFromEnv :: IO String
authTokenFromEnv = getEnv "GH_AUTHTOKEN"

-- | TODO: Make this configurable more easily / dynamic by user
repositoryFromEnv :: IO String
repositoryFromEnv = getEnv "GH_REPOSITORY"

requestGithub :: String -> IO LazyByteString
requestGithub query = do
    initial <- parseRequest "POST https://api.github.com/graphql"
    token <- authTokenFromEnv
    manager <- newTlsManager
    let body = object [ "query" .= query ]
    let request = initial { requestBody = RequestBodyLBS $ encode body, requestHeaders =
        [ (hAuthorization, ByteString.pack ("Bearer " ++ token) )
        , (hAccept, ByteString.pack "application/vnd.github+json")
        , (hContentType, ByteString.pack "application/json")
        , (hUserAgent, ByteString.pack "rhine-streamdeck-client")
        , ("X-GitHub-Api-Version", ByteString.pack "2022-11-28")
        ]}
    resp <- httpLbs request manager
    pure $ responseBody resp

queryPullRequests :: String -> String
queryPullRequests ownerAndRepo =
    let [owner, repo] = Text.splitOn "/" $ Text.pack ownerAndRepo
    in "\
\query {\
\  repository(owner: \"" <> Text.unpack owner <> "\", name: \"" <> Text.unpack repo <> "\") {\
\    rulesets (first: 5) {\
\      nodes {\
\        name\
\        rules (first: 10) {\
\          nodes {\
\            type\
\            parameters {\
\              ... on PullRequestParameters {\
\                requiredApprovingReviewCount\
\              }\
\            }\
\          }\
\        }\
\      }\
\    }\
\    \
\    pullRequests (last: 10, states: [OPEN]) {\
\      nodes {\
\        title\
\        number\
\        url\
\        reviews (last: 20) {\
\          nodes {\
\            state\
\            author { login }\
\          }\
\        }\
\      }\
\    }\
\  }\
\}\
\"
