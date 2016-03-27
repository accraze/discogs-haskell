-- | Contains label related actions, like finding a label by id or getting a list of all the label's releases.
module Discogs.Actions.Label
  ( getLabel, getLabelReleases ) where

import Discogs.Types.Label
import Discogs.Types.Discogs
import qualified Discogs.Routes.Label as Route

import Control.Monad
import Data.Default.Class
import Data.Aeson
import qualified Data.Text as Text

-- | Get a label with a specific id
--
-- GET \/labels\/:labelId
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getLabel $ LabelID "1"
-- @
getLabel :: Monad m => LabelID -> DiscogsT m Label
getLabel = runRoute . Route.getLabel

-- | Get all releases from a label with a specific id
--
-- GET \/labels\/:labelId\/releases
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getLabelReleases $ LabelID "1"
-- @
getLabelReleases :: Monad m => LabelID -> DiscogsT m LabelReleaseList
getLabelReleases = runRoute . Route.getLabelReleases