module Discogs.Actions () where

import Discogs.Types.Artist
import Discogs.Types.Release
import Discogs.Types.Discogs
--import Network.API.Builder
import Discogs.Routes.Artist as Route
import Discogs.Routes.Release as Route

import Discogs.Actions.Artist
import Discogs.Actions.Release

-- | Get the info Discogs exposes on artist behind the specified artist id
getArtist :: Monad m => ArtistID -> DiscogsT m Artist
getArtist = runRoute . Route.getArtist

-- | Get the all releases from an artist with the specified artist id
getArtistReleases :: Monad m => ArtistID -> DiscogsT m Artist
getArtistReleases = runRoute . Route.getArtistReleases

---- | Get the info Discogs exposes on release behind the specified release id
getRelease :: Monad m => ReleaseID -> DiscogsT m Release
getRelease = runRoute . Route.getRelease


--module Discogs.Actions
--  ( module Discogs.Actions.Artist
--  , module Discogs.Actions.Release ) where

--import Discogs.Actions.Artist
--import Discogs.Actions.Release