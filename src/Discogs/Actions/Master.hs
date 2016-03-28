-- | Contains master-related actions, like finding a specific master release by id 
--   or getting a list of all master versions.
module Discogs.Actions.Master
  ( getMaster,
    getMasterVersions ) where

import Discogs.Types.Master
import Discogs.Types.Discogs
import qualified Discogs.Routes.Master as Route

import Control.Monad
import Data.Default.Class
import Data.Aeson
import qualified Data.Text as Text

-- | Get the information on a master release with the specified id
--
-- GET \/masters\/:masterId
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getMaster $ MasterID "1000"
-- @
getMaster :: Monad m => MasterID -> DiscogsT m Master
getMaster = runRoute . Route.getMaster

-- | Get a list of all master versions with a specific id
--
-- GET \/masters\/:masterId\/versions
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getMasterVersions $ MasterID "1000"
-- @
getMasterVersions :: Monad m => MasterID -> DiscogsT m MasterVersionsList
getMasterVersions = runRoute . Route.getMasterVersions