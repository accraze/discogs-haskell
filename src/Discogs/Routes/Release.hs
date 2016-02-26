module Discogs.Routes.Release where

import Discogs.Types.Release

import Network.API.Builder.Routes

getRelease :: ReleaseID -> Route
getRelease (ReleaseID release) = Route [ "releases", release ]
                                  []
                                  "GET"
