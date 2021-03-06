{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Release where

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
import Discogs.Types.Pagination

-- | This is required to look up a release. Example: \'249504\'
newtype ReleaseID = ReleaseID Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON ReleaseID

-- | The Release resource represents a particular physical or digital object 
-- released by one or more Artists.
data Release =
  Release { 
        title :: Text
       , id :: Int
       , artists :: !Array
       , data_quality :: Text
       , thumb :: Maybe Text
       , community :: Community
       , companies :: Maybe Array
       , country :: Maybe Text
       , date_added :: Maybe Text
       , date_changed :: Maybe Text
       , estimated_weight :: Maybe Int
       , extraartists :: Maybe Array
       , format_quantity :: Maybe Int
       , formats :: Array
       , genres :: [Text]
       , identifiers :: Maybe Array
       , images :: Maybe Array
       , labels :: Maybe Array
       , master_id :: Int
       , master_url :: Text
       , notes :: Text
       , released :: Maybe Text
       , released_formatted :: Maybe Text
       , resource_url :: Maybe Text
       , series :: Maybe Array
       , status :: Text
       , styles :: [Text]
       , tracklist :: !Array
       , uri :: Text  
       , videos :: Maybe Array
       , year :: Maybe Int }
  deriving (Show, Eq, Generic)

instance FromJSON Release

-- | This is a list containg Releases.
data ReleaseList
    = ReleaseList {
          releases    :: !Array
         ,pagination  :: Pagination
        } deriving Show

instance FromJSON ReleaseList where
  parseJSON (Object o) = ReleaseList <$>
                        (o .: "releases")
                        <*> (o .: "pagination")
  parseJSON _ = mzero

-- | This is an artist who performs on a release.
data ArtistRelease
    = ArtistRelease { artist           :: String
                      ,r_id            :: Int
                      ,main_release    :: Int
                      ,rResource_url   :: String
                      ,role            :: String
                      ,rthumb          :: String
                      ,rtitle          :: String
                      ,rtype           :: String
                      ,ryear            :: Int
                    } deriving Show

instance FromJSON ArtistRelease where
  parseJSON (Object o) =
    ArtistRelease <$> (o .: "artist")
              <*> (o .: "id")
              <*> (o .: "main_release")
              <*> (o .: "resource_url")
              <*> (o .: "role")
              <*> (o .: "thumb")
              <*> (o .: "title")
              <*> (o .: "type")
              <*> (o .: "year")
  parseJSON _ = mzero

-- | This is the format of a release. Example: 2 X 7" Vinyl 
data Format
    = Format {  descriptions  :: Maybe Array
                ,name    :: Int
                ,quantity :: Int
            } deriving (Show, Generic, Eq)

instance FromJSON Format

-- | This is the Release's identifier (barcode, etc.)
data Identifier
    = Identifier {  iType  :: String
                 , value   :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Identifier where
  parseJSON (Object o) =
    Identifier <$> (o .: "type")
              <*> (o .: "value")
  parseJSON _ = mzero

-- | This is a track contained on a Release.
data Track
    = Track {  duration  :: String
            ,  position  :: String
            ,  tTitle   :: String
            ,  type_   :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Track

-- | This is a Video resource associated with a Release.
data Video
    = Video {  v_description  :: String
             , v_duration   :: Int
              , embed   :: Bool
              , v_title :: String
              , v_uri :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Video where
  parseJSON (Object o) =
    Video <$> (o .: "description")
              <*> (o .: "duration")
              <*> (o .: "embed")
              <*> (o .: "title")
              <*> (o .: "uri")
  parseJSON _ = mzero