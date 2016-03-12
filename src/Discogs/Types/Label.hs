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
    --parseJSON (Object o) = Label <$> o .: "id"
    --                             <*> o .: "profile"
    --                             <*> o .: "releases_url"
    --                             <*> o .: "name"
    --                             <*> o .: "contact_info"
    --                             <*> o .: "resource_url"
    --                             <*> o .: "uri"
    --                             <*> o .: "sublabels"
    --                             <*> o .: "urls"
    --                             <*> o .: "images"
    --                             <*> o .: "resource_url"
    --                             <*> o .: "data_quality"
    --parseJSON _ = mempty

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