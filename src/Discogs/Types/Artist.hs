{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Artist where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype ArtistID = ArtistID Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON ArtistID


data Artist =
  Artist { 
        profile :: Maybe Text
       , id :: Int
       , releases_url :: Text
       , resource_url :: Maybe Text
       , uri :: Maybe Text
       , data_quality :: Text
       , namevariations :: Maybe [Text]
       , urls :: [Text] 
       , images :: !Array
       , members :: !Array }
  deriving (Show, Eq, Read, Generic)

instance FromJSON Artist


data ImagesList = ImagesList {imagesList :: !Array}
    deriving (Show, Eq, Generic)

instance FromJSON ImagesList

data Image = Image {height :: Int
                , iResource_url :: String
                , iType :: String
                , iUri :: String
                , uri150 :: String
                , width :: Int }
                deriving (Show, Eq, Generic)

instance FromJSON Image where
    parseJSON (Object o) = Image <$> o .: "height" 
                                 <*> o .: "resource_url"
                                 <*> o .: "type"
                                 <*> o .: "uri"
                                 <*> o .: "uri150"
                                 <*> o .: "width"
    parseJSON _ = mempty

data MembersList 
        = MembersList {
                    membersList :: !Array
                } deriving (Show, Eq)

instance FromJSON MembersList where
    parseJSON (Object o) = MembersList <$> (o .: "members")
    parseJSON _ = mzero

data Member = Member {active :: Bool
                , id2 :: Integer
                , name :: String
                , mResource_url :: String }
                deriving (Show, Eq, Generic)

instance FromJSON Member

data ReleaseArtistList
        = ReleaseArtistList {
                    releaseArtists :: !Array
                } deriving (Show, Eq)

instance FromJSON ReleaseArtistList where
    parseJSON (Object o) = ReleaseArtistList <$> (o .: "artists")
    parseJSON _ = mzero

data ReleaseArtist = ReleaseArtist {anv :: String
                , rId :: Int
                , join :: String
                , rName :: String
                , rResource_url :: String
                , role :: String
                , tracks :: String }
                deriving (Show, Eq, Generic)

instance FromJSON ReleaseArtist