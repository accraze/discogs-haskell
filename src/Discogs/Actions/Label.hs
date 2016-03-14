-- | Contains master-related actions, like finding a label
module Discogs.Actions.Label
  ( getLabel, getLabelReleases ) where

import Discogs.Types.Label
import Discogs.Types.Discogs
import qualified Discogs.Routes.Label as Route

import Control.Monad
import Data.Default.Class
import Data.Aeson
--import Network.API.Builder
import qualified Data.Text as Text

-- | Get the information Discogs exposes on master with the specified id
getLabel :: Monad m => LabelID -> DiscogsT m Label
getLabel = runRoute . Route.getLabel

-- | Get the information Discogs exposes on labl releases with the specified id
getLabelReleases :: Monad m => LabelID -> DiscogsT m LabelReleaseList
getLabelReleases = runRoute . Route.getLabelReleases