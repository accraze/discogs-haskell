-- | Contains master-related actions, like finding a label
module Discogs.Actions.Label
  ( getLabel ) where

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