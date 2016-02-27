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
--instance FromJSON ArtistID where
--  parseJSON (String s) = return $ ArtistID s
--  parseJSON _ = mempty

data Artist =
  Artist { 
        profile :: Maybe Text
       , releases_url :: Text
       , resource_url :: Maybe Text
       , uri :: Maybe Text
       , data_quality :: Text
       , namevariations :: Maybe [Text]
       , urls :: [Text] 
       , images :: !Array
       , members :: !Array }
  deriving (Show, Eq, Generic)

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
                } deriving Show

instance FromJSON MembersList where
    parseJSON (Object o) = MembersList <$> (o .: "members")
    parseJSON _ = mzero

data Member = Member {active :: Bool
                , id2 :: Integer
                , name :: String
                , mResource_url :: String }
                deriving (Show, Eq, Generic)

instance FromJSON Member