{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.User where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

import Discogs.Types.Artist


data Community
    = Community {
             contributors :: !Array
             , data_quality :: String
             , have :: Int
             , rating :: Rating
             , status :: String
             , submitter :: Contributor
             , want :: Int
        } deriving (Show, Eq, Generic)

instance FromJSON Community

data Contributor
    = Contributor {  
            resource_url  :: String
            , username    :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Contributor

data Rating
    = Rating {  
            average  :: Double
            , count  :: Int
            } deriving (Show, Generic, Eq)

instance FromJSON Rating

