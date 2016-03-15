-- | Contains master-related actions, like finding master release
module Discogs.Actions.Master
  ( getMaster,
    getMasterVersions ) where

import Discogs.Types.Master
import Discogs.Types.Discogs
import qualified Discogs.Routes.Master as Route

import Control.Monad
import Data.Default.Class
import Data.Aeson
--import Network.API.Builder
import qualified Data.Text as Text

-- | Get the information Discogs exposes on master with the specified id
getMaster :: Monad m => MasterID -> DiscogsT m Master
getMaster = runRoute . Route.getMaster

-- | Get the information Discogs exposes on master versions with the specified id
getMasterVersions :: Monad m => MasterID -> DiscogsT m MasterVersionsList
getMasterVersions = runRoute . Route.getMasterVersions