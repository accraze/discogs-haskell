module Discogs.Types.Artist where

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype ArtistID = ArtistID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON ArtistID where
  parseJSON (String s) = return $ ArtistID s
  parseJSON _ = mempty

data Artist =
  Artist { artistID :: ArtistID
       , profile :: Maybe Text
       , releases_url :: Text
       , resource_url :: Maybe Text
       , uri :: Maybe Text
       , data_quality :: Text
       , namevariations :: Maybe [Text]
       , urls :: [Text]
       , images :: ImagesList
       , members :: Maybe MembersList }
  deriving (Show, Eq)

instance FromJSON Artist where
  parseJSON (Object o) = Artist <$> o .: "artistId"
                              <*> o .: "profile"
                              <*> o .: "releases_url"
                              <*> o .:? "resource_url"
                              <*> o .:? "uri"
                              <*> o .: "data_quality"
                              <*> o .:? "namevariations"
                              <*> o .: "urls"
                              <*> o .: "images"
                              <*> o .:? "members"
  parseJSON _ = mempty


newtype ImagesList = ImagesList {imagesList :: [Image]}
    deriving (Show, Eq)

instance FromJSON ImagesList where
    parseJSON (Object o) = ImagesList <$> o .: "images"
    parseJSON _ = mempty

data Image = Image {height :: Int
                , iResource_url :: String
                , typ :: String
                , iUri :: String
                , uri150 :: String
                , width :: Integer }
                deriving (Show, Eq)

instance FromJSON Image where
    parseJSON (Object o) = Image <$> o .: "height" 
                                 <*> o .: "resource_url"
                                 <*> o .: "type"
                                 <*> o .: "uri"
                                 <*> o .: "uri150"
                                 <*> o .: "width"
    parseJSON _ = mempty

newtype MembersList = MembersList {membersList :: [Member]}
    deriving (Show, Eq)

instance FromJSON MembersList where
    parseJSON (Object o) = MembersList <$> o .: "members"
    parseJSON _ = mempty

data Member = Member {active :: Bool
                , id :: Integer
                , name :: String
                , mResource_url :: String }
                deriving (Show, Eq)

instance FromJSON Member where
    parseJSON (Object o) = Member <$> o .: "active" 
                                 <*> o .: "id"
                                 <*> o .: "name"
                                 <*> o .: "resource_url"
    parseJSON _ = mempty