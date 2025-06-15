module Github.GithubClock where

import Prelude

data GithubClock = GithubClock

data GithubPullRequest = GithubPullRequest
    { title :: String
    , url :: String }
    deriving stock (Show)

instance (MonadIO m) => Clock m GithubClock 
    where
    type Time GithubClock = UTCTime
    type Tag GithubClock = [GithubPullRequest]

    initClock :: GithubClock -> RunningClockInit m (Time GithubClock) (Tag GithubClock)
    initClock GithubClock = do
        (x, initialTime)  <- initClock $ ioClock @m $ waitClock @1000

        let clock = concatS @m $ proc () -> do
                (time, _) <- x -< ()
                returnA -< pure (time, [])
        pure (clock, initialTime)

instance GetClockProxy GithubClock
