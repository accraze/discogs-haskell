-- | Contains release related actions, like finding a specific release by id.
module Discogs.Actions.Release
  ( getRelease ) where

import Discogs.Types.Release
import Discogs.Types.Discogs
import qualified Discogs.Routes.Release as Route

import Control.Monad
import Data.Default.Class
import Data.Text (Text)
import qualified Data.Text as Text

-- | Get release with the specified id
--
-- GET \/releases\/:releaseId
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getRelease $ ReleaseID "249504"
-- @
getRelease :: Monad m => ReleaseID -> DiscogsT m Release
getRelease = runRoute . Route.getRelease
