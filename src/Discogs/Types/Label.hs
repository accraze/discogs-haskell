{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Label where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

import Discogs.Types.Artist
import Discogs.Types.Pagination

-- | A company that was involved with a specific Label.
data Company
    = Company {  
             catno  :: String
            , entity_type    :: String
            , entity_type_name :: Maybe String
            , company_id :: Int
            , company_name :: String
            , company_resource_url :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Company where 
    parseJSON (Object o) = Company <$> o .: "catno" 
                                   <*> o .: "entity_type"
                                   <*> o .: "entity_type_name"
                                   <*> o .: "id"
                                   <*> o .: "name"
                                   <*> o .: "resource_url"
    parseJSON _ = mempty

newtype LabelID = LabelID Text
  deriving (Show, Read, Eq, Ord, Generic)
instance FromJSON LabelID

-- | The Label resource represents a label, company, recording studio, 
-- location, or other entity involved with Artists and Releases. 
data Label
    = Label {
             id :: Int
            , profile  :: String
            , releases_url :: String
            , name :: String
            , contact_info :: String
            , uri :: String
            , sublabels :: Array
            , urls :: Array
            , images:: Array
            , resource_url :: String
            , data_quality :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Label

-- | This is for any sort of associated sub-label, siblings, etc.
data Sublabel
    = Sublabel {
            sublabel_id :: Int
            , sublabel_name :: String
            , sublabel_resource_url :: String
            } deriving (Show, Generic, Eq)   

instance FromJSON Sublabel where
    parseJSON (Object o) = Sublabel <$> o .: "id" 
                                 <*> o .: "name"
                                 <*> o .: "resource_url"
    parseJSON _ = mempty

-- | A list of type LabelRelease.
data LabelReleaseList
    = LabelReleaseList {  
                    pagination  :: Pagination
                    ,releases   :: !Array
                    } deriving (Show, Generic, Eq)

instance FromJSON LabelReleaseList

-- | A release that was put out by a Label.
data LabelRelease
    = LabelRelease {  artist              :: String
                    ,release_catno        :: String
                    ,format               :: String
                    ,release_id           :: String
                    ,release_resource_url :: String
                    ,status               :: String
                    ,thumb                :: String
                    ,title                :: String
                    ,year                 :: Int
                    } deriving (Show, Generic, Eq)

instance FromJSON LabelRelease where
    parseJSON (Object o) = LabelRelease <$> o .: "artist" 
                             <*> o .: "catno"
                             <*> o .: "format"
                             <*> o .: "release_id"
                             <*> o .: "resource_url"
                             <*> o .: "status"
                             <*> o .: "thumb"
                             <*> o .: "title"
                             <*> o .: "year"
    parseJSON _ = mempty