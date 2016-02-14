-- | Contains artist-related actions, like finding releases or retrieving an
--   artist's information.
module Discogs.Actions.Artist
  ( getArtist
  , getArtistReleases ) where

import Discogs.Types.Artist
import Discogs.Types.Discogs
import qualified Discogs.Routes.Artist as Route

import Control.Monad
import Data.Default.Class
import Data.Text (Text)
--import Network.API.Builder
import qualified Data.Text as Text

-- | Get the information Discogs exposes on artist with the specified id
getArtist :: Monad m => ArtistID -> DiscogsT m Artist
getArtist = runRoute . Route.getArtist

-- | Get all release information exposed for an artist with specified id
getArtistReleases :: Monad m => ArtistID -> DiscogsT m Artist
getArtistReleases = runRoute . Route.getArtistReleases

