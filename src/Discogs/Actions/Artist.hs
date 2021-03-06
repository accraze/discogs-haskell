-- | Contains artist-related actions, like finding an artist's releases or retrieving an
--   artist's information.
module Discogs.Actions.Artist
  ( getArtist
  , getArtistReleases ) where

import Discogs.Types.Artist
import Discogs.Types.Discogs
import Discogs.Types.Release
import qualified Discogs.Routes.Artist as Route

import Control.Monad
import Data.Default.Class
import Data.Text (Text)
import qualified Data.Text as Text

-- | Get the information Discogs exposes on artist with the specified id
--
-- GET \/artists\/:artistId
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getArtist $ ArtistID "108713"
-- @
getArtist :: Monad m => ArtistID -> DiscogsT m Artist
getArtist = runRoute . Route.getArtist

-- | Get all release information exposed for an artist with specified id
--
-- GET \/artists\/:artistId\/releases
--
-- @
--     runDiscogsAnon $ Discogs.Actions.getArtistReleases $ ArtistID "108713"
-- @
getArtistReleases :: Monad m => ArtistID -> DiscogsT m ReleaseList
getArtistReleases = runRoute . Route.getArtistReleases

