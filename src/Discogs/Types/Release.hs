{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Release where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype ReleaseID = ReleaseID Text
  deriving (Show, Read, Eq, Ord, Generic)


instance FromJSON ReleaseID

data Release =
  Release { 
        title :: Text
       , data_quality :: Text
       , thumb :: Maybe Text
       , country :: Maybe Text
       , master_url :: Text 
       , uri :: Text 
       , notes :: Text }
       --, images :: ImagesList
       --, members :: Maybe MembersList }
  deriving (Show, Eq, Generic)

instance FromJSON Release


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

data ArtistRelease
    = ArtistRelease { artist           :: String
                      ,id              :: Int
                      ,main_release    :: Int
                      ,resource_url    :: String
                      ,role            :: String
                      ,rthumb           :: String
                      ,rtitle           :: String
                      ,rtype            :: String
                      ,year            :: Int
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


data Pagination
    = Pagination {  per_page  :: Int
                    ,items    :: Int
                    ,page     :: Int
                    ,pages    :: Int
                    } deriving (Show, Generic, Eq)

instance FromJSON Pagination
