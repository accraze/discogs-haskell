-- | Contains release-related actions, like finding releases or retrieving an
--   artist's information.
module Discogs.Actions.Release
  ( getRelease ) where

import Discogs.Types.Release
import Discogs.Types.Discogs
import qualified Discogs.Routes.Release as Route

import Control.Monad
import Data.Default.Class
import Data.Text (Text)
import qualified Data.Text as Text

-- | Get the information Discogs exposes on release with the specified id
getRelease :: Monad m => ReleaseID -> DiscogsT m Release
getRelease = runRoute . Route.getRelease
