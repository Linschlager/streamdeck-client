module Buttons.GithubPrButton where

import Prelude
import FontToImage qualified
import SvgImage qualified
import FontToImage (TextAlignment(..))
import Actions.GithubPrStatus (PullRequest(..), PullRequestState(..))
import Actions.GithubPrStatus qualified as GithubPrStatus

update :: forall s m. (MonadIO m, IsStreamDeckWithDisplayButtons s) => PullRequest -> m (Drawing PixelRGBA8 ())
update pr = do
    font <- liftIO FontToImage.loadFont
    info <- liftIO $ GithubPrStatus.pullRequestReviews pr.number
    traceShowM info

    let numReviews = show (length $ filter ((== Approved) . (.state)) info) <> "/" <> show (length info) <> " approvals"

    pure $
        SvgImage.mergeImage
            (FontToImage.textToImage @s font numReviews TopLeft)
            (FontToImage.textToImage @s font pr.title BottomLeft)
