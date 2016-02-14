module Discogs.Actions () where

import Discogs.Types.Artist
import Discogs.Types.Discogs
--import Network.API.Builder
import Discogs.Routes.Artist as Route

-- | Get the info Discogs exposes on artist behind the specified artist id
getArtist :: Monad m => ArtistID -> DiscogsT m Artist
getArtist = runRoute . Route.getArtist

-- | Get the all releases from an artist with the specified artist id
getArtistReleases :: Monad m => ArtistID -> DiscogsT m Artist
getArtistReleases = runRoute . Route.getArtistReleases