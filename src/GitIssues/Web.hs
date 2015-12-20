{-# LANGUAGE OverloadedStrings #-}
module GitIssues.Web
  where

import           Control.Monad          (forM)
import           Control.Monad.IO.Class
import           Data.List              (find)
import           Data.Monoid
import           Data.Text              (pack)
import           GitIssues.Types
import           GitIssues.Web.Styles
import           Lucid
import           Lucid.Base
import           Lucid.Bootstrap
import           System.FilePath
import           Web.Spock

template :: Monad m => HtmlT m a -> HtmlT m a
template content = doctypehtml_ $ do
    head_ $ do
        title_ "Issues"
        link_ [ rel_ "stylesheet"
              , type_ "text/css"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
              , makeAttribute "crossorigin" "anonymous"
              ]
        style_ [] css
    body_ content

renderLucid :: MonadIO m => Html a1 -> ActionT m ()
renderLucid = lazyBytes . renderBS

gitIssuesServer :: MonadIO m => (FilePath, String) -> SpockT m ()
gitIssuesServer (home, repo) = do
    get "/" $ do
        store <- liftIO $ readOrCreateStore repo
        setHeader "content-type" "text/html"
        renderLucid $ template $
            container_ $ do
                row_ $ h1_ (toHtml (takeBaseName repo))

                row_ [class_ "issue-button-row"] $ do
                    button_ [class_ "btn btn-default"] "Open"
                    button_ [class_ "btn btn-default"] "Closed"
                    button_ [class_ "btn btn-primary"] "New Issue"

                row_ $ ul_ [class_ "list-group"] $ forM (storeIssues store) $ \issue ->
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
            Just issue -> renderLucid $ template $
                container_ [] $ do
                    row_ $ do
                        h1_ [ class_ ".issue-view-title"
                            ] (toHtml (issueTitle issue))
                        a_ [ href_ ("/" <> pack (show (issueNumber issue)) <> "/edit")
                           , class_ "spacing-right btn btn-primary"
                           ] "Edit"
                        a_ [ href_ ("/" <> pack (show (issueNumber issue)) <> "/delete")
                           , class_ "btn btn-danger"
                           ] "Delete"
                    p_ [] (toHtml (issueBody issue))
            Nothing -> error "Not found"

    post ("/" <//> var <//> "edit") $ \n -> do
        store <- liftIO $ readOrCreateStore repo
        r <- body
        liftIO $ print r
        redirect ("/" <> pack n)

    get ("/" <//> var <//> "edit") $ \n -> do
        store <- liftIO $ readOrCreateStore repo
        setHeader "content-type" "text/html"
        let missue = find ((== n) . issueNumber) (storeIssues store)
        case missue of
            Just issue -> renderLucid $ template $
                container_ [class_ "spacing-top"] $
                    form_ [ action_
                               ("/" <> pack (show (issueNumber issue)) <> "/edit")
                          , method_ "post"
                          ] $ do
                        div_ [class_ "form-group"] $
                            input_ [ type_ "text"
                                   , class_ "form-control"
                                   , name_ "title"
                                   , value_  (pack (issueTitle issue))
                                   ]

                        div_ [class_ "form-group"] $
                            textarea_ [ class_ "issue-edit-body form-control"
                                      , name_ "body"
                                      , rows_ "6"
                                      ] (toHtml (issueBody issue))

                        button_ [ type_ "submit"
                                , class_ "spacing-right btn btn-primary"
                                ] "Save"

                        a_ [ href_ ("/" <> pack (show (issueNumber issue)))
                           , class_ "btn btn-danger"
                           ] "Cancel"
            Nothing -> error "Not found"
