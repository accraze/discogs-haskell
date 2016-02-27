module Discogs.Routes.Artist where

import Discogs.Types.Artist
import Discogs.Types.Release

import Network.API.Builder.Routes

getArtist :: ArtistID -> Route
getArtist (ArtistID artist) = Route [ "artists", artist ]
                                  []
                                  "GET"

getArtistReleases :: ArtistID -> Route
getArtistReleases (ArtistID artist) = Route [ "artists", artist, "releases" ]
                                  []
                                  "GET"