{-# LANGUAGE DeriveGeneric #-}

module Discogs.Types.Label where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

data Company
    = Company {  
             catno  :: String
            , entity_type    :: String
            , entity_type_name :: Maybe String
            , id :: Int
            , name :: String
            , resource_url :: String
            } deriving (Show, Generic, Eq)

instance FromJSON Company