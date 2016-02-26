{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Release where

import GHC.Generics

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype ReleaseID = ReleaseID Text
  deriving (Show, Read, Eq, Ord, Generic)


instance FromJSON ReleaseID
--instance FromJSON ReleaseID where
--  parseJSON (String s) = return $ ReleaseID s
--  parseJSON _ = mempty

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