{-# LANGUAGE OverloadedStrings #-}
module GitIssues.Web.Styles
  where

import           Clay
import           Data.Text.Lazy (Text)
import           Prelude        hiding ((**))

css :: Text
css = render stylesheet

stylesheet :: Css
stylesheet = do
    ".spacing-top" ?
        marginTop (px 10)

    ".spacing-right" ?
        marginRight (px 10)

    ".issue-button-row" ? do
        marginTop (px 10)
        marginBottom (px 10)

    ".issue-button-row" ** ".btn" ?
        marginRight (px 10)
