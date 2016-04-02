{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Artist where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

-- | This is required to look up an artist. Example: \'108713\'
newtype ArtistID = ArtistID Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON ArtistID

-- | The Artist resource represents a person in the Discogs database who contributed to a Release in some capacity.
data Artist =
  Artist { 
        profile :: Maybe Text -- Artist profile info
       , id :: Int -- The Artist ID
       , releases_url :: Text -- Url with Artist's releases 
       , resource_url :: Maybe Text -- Url with Artist resources
       , uri :: Maybe Text
       , data_quality :: Text 
       , namevariations :: Maybe [Text] -- Different names for the Artist.
       , urls :: [Text]  -- 
       , images :: !Array
       , members :: !Array }
  deriving (Show, Eq, Read, Generic)

instance FromJSON Artist

-- | This is a list of type Image.
data ImagesList = ImagesList {imagesList :: !Array}
    deriving (Show, Eq, Generic)

instance FromJSON ImagesList

-- | This is a image of an Artist which has been submitted by a contributor.
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

-- | This is a list of type Member.
data MembersList 
        = MembersList {
                    membersList :: !Array
                } deriving (Show, Eq)

instance FromJSON MembersList where
    parseJSON (Object o) = MembersList <$> (o .: "members")
    parseJSON _ = mzero

-- | This is a member that belongs to the Artists. Eg: members of a band, contributors to a project.
data Member = Member {active :: Bool
                , id2 :: Integer
                , name :: String
                , mResource_url :: String }
                deriving (Show, Eq, Generic)

instance FromJSON Member

-- | This is a list of type ReleaseArtist
data ReleaseArtistList
        = ReleaseArtistList {
                    releaseArtists :: !Array
                } deriving (Show, Eq)

instance FromJSON ReleaseArtistList where
    parseJSON (Object o) = ReleaseArtistList <$> (o .: "artists")
    parseJSON _ = mzero

-- | This a Release that the Artist has performed on.
data ReleaseArtist = ReleaseArtist {anv :: String
                , rId :: Int
                , join :: String
                , rName :: String
                , rResource_url :: String
                , role :: String
                , tracks :: String }
                deriving (Show, Eq, Generic)

instance FromJSON ReleaseArtist