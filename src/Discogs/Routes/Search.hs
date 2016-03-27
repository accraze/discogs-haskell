module Discogs.Routes.Search where

import Data.Maybe
import Data.Text (Text)
import Network.API.Builder.Routes

--searchRoute :: Maybe SubredditName -> Options PostID -> Search.Order -> Maybe Text -> Text -> Route
--searchRoute r opts sorder engine q =
--  Route (path r)
--        [ "after" =. after opts
--        , "before" =. before opts
--        , "restrict_sr" =. isJust r
--        , "sort" =. sorder
--        , "syntax" =. engine
--        , "limit" =. limit opts
--        , "q" =. Just q ]
--        "GET"
--  where
----    path (Just (R sub)) = [ "r", sub, "search" ]
--    path Nothing = [ "search" ]



searchRoute :: Maybe Text -> Route
searchRoute q =
  Route (path )
        [ "q" =. Just q ]
        "GET"
  where
    path = [ "search" ]