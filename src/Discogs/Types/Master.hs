{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Master where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

import Discogs.Types.Artist
import Discogs.Types.Label
import Discogs.Types.User
import Discogs.Types.Release
import Discogs.Types.Pagination

-- | This is required to look up a master release. Example: \'1000\'
data MasterID = MasterID Text
  deriving (Show, Read, Eq, Ord, Generic)


instance FromJSON MasterID

-- | The Master resource represents a set of similar Releases. 
-- Masters (also known as “master releases”) have a “main release” which is 
-- often the chronologically earliest.
data Master =
  Master { 
        title :: Text
       , id :: Int
       , artists :: !Array
       , data_quality :: Text
       , genres :: [Text]
       , images :: Maybe Array
       , main_release :: Int
       , main_release_url :: Text
       , resource_url :: Maybe Text
       , styles :: [Text]
       , tracklist :: !Array
       , uri :: Text  
       , videos :: Maybe Array
       , versions_url :: Text
       , year :: Maybe Int }
  deriving (Show, Eq, Generic)

instance FromJSON Master

-- | This is a list of type MasterVersion.
data MasterVersionsList =
  MasterVersionsList { 
       pagination :: Pagination
       , versions :: !Array }
  deriving (Show, Eq, Generic)

instance FromJSON MasterVersionsList