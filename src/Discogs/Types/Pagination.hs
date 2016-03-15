{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Pagination where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

data Pagination
    = Pagination {  per_page  :: Int
                    ,items    :: Int
                    ,page     :: Int
                    ,pages    :: Int
                    ,urls     :: Urls
                    } deriving (Show, Generic, Eq)

instance FromJSON Pagination

data Urls
    = Urls {  last     :: Maybe String
              ,next    :: Maybe String
            } deriving (Show, Generic, Eq)

instance FromJSON Urls