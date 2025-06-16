module Github.GithubClock where

import Github.GithubQueries
import Github.Types
import Prelude

data GithubClock = GithubClock

fetchGithub :: IO LazyByteString
fetchGithub = do
    repo <- repositoryFromEnv
    requestGithub . queryPullRequests $ repo

clockContent :: MonadIO m => RunningClock m UTCTime (Maybe Double) -> Automaton m () [(Time GithubClock, Tag GithubClock)]
clockContent base = proc () -> do
                (time, _) <- base -< ()
                result <- constM . liftIO $ fetchGithub -< ()
                let prs = either error id $ eitherDecode @RepositoryResponse result
                returnA -< pure (time, prs)

instance (MonadIO m) => Clock m GithubClock
    where
    type Time GithubClock = UTCTime
    type Tag GithubClock = RepositoryResponse

    initClock :: GithubClock -> RunningClockInit m (Time GithubClock) (Tag GithubClock)
    initClock GithubClock = do
        (x, initialTime)  <- initClock $ ioClock @m $ waitClock @1000
        let clock = concatS @m $ clockContent x
        pure (clock, initialTime)

instance GetClockProxy GithubClock
