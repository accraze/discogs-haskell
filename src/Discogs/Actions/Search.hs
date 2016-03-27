module Discogs.Actions.Search where

import Discogs.Routes.Search
import Discogs.Types.Discogs
import Discogs.Types.SearchResult

import Data.Text (Text)

search :: Monad m => Maybe Text -> DiscogsT m SearchResultList
search query =
  runRoute $ searchRoute query
