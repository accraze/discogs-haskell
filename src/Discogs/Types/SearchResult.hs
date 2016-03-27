{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.SearchResult where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

import Discogs.Types.Artist
import Discogs.Types.Pagination

data SearchResultList
    = SearchResultList {
          results    :: !Array
         ,pagination  :: Pagination
        } deriving Show

instance FromJSON SearchResultList where
  parseJSON (Object o) = SearchResultList <$>
                        (o .: "releases")
                        <*> (o .: "pagination")
  parseJSON _ = mzero

data CommunityInfo
    = CommunityInfo {
          want  :: Int
         ,have  :: Int
        } deriving (Show, Generic)

instance FromJSON CommunityInfo


data SearchResult
    = SearchResult {
          style  :: [Text]
         ,thumb  :: Text
         ,title  :: Text
         ,country  :: Text
         ,format  :: [Text]
         ,uri  :: Text
         ,community  :: CommunityInfo
         ,label  :: [Text]
         ,catno  :: Text
         ,year  :: Text
         ,genre  :: [Text]
         ,resource_url :: Text
         ,rtype :: Text
         ,id :: Int
        } deriving (Show, Generic)

instance FromJSON SearchResult

