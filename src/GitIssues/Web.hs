{-# LANGUAGE OverloadedStrings #-}
module GitIssues.Web
  where

import           Control.Monad          (forM)
import           Control.Monad.IO.Class
import           Data.List              (find)
import           Data.Monoid
import           Data.Text              (pack)
import           GitIssues.Types
import           Lucid
import           Lucid.Base
import           Lucid.Bootstrap
import           System.FilePath
import           Web.Spock

template :: Monad m => HtmlT m a -> HtmlT m a
template content = doctypehtml_ $ do
    head_ $ do
        title_ "git-issues"
        link_ [ rel_ "stylesheet"
              , type_ "text/css"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
              , makeAttribute "crossorigin" "anonymous"
              ]
    body_ content

renderLucid :: MonadIO m => Html a1 -> ActionT m ()
renderLucid = lazyBytes . renderBS

gitIssuesServer :: MonadIO m => (FilePath, String) -> SpockT m ()
gitIssuesServer (home, repo) = do
    get "/" $ do
        store <- liftIO $ readOrCreateStore repo
        setHeader "content-type" "text/html"
        renderLucid $ template $ do
            container_ [style_ "text-align: center;"] $ do
                h1_ "git-issues"
                h2_ (toHtml $ "Using repository: " <> makeRelative home repo)
            container_ [style_ "text-align: center;"] $
                ul_ [class_ "list-group"] $ forM (storeIssues store) $ \issue ->
                    a_ [ href_ ("/" <> pack (show (issueNumber issue)))
                       ] $
                        li_ [class_ "list-group-item"] $ do
                            toHtml $ "#" <> show (issueNumber issue)
                            " - "
                            toHtml $ issueTitle issue

    get ("/" <//> var) $ \n -> do
        store <- liftIO $ readOrCreateStore repo
        setHeader "content-type" "text/html"
        let missue = find ((== n) . issueNumber) (storeIssues store)
        case missue of
            Just issue -> text (pack (issueTitle issue))
            Nothing -> error "Not found"
