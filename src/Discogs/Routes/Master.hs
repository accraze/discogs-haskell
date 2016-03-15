module Discogs.Routes.Master where

import Discogs.Types.Master

import Network.API.Builder.Routes

getMaster :: MasterID -> Route
getMaster (MasterID master) = Route [ "masters", master ]
                                  []
                                  "GET"

getMasterVersions :: MasterID -> Route
getMasterVersions (MasterID master) = Route [ "masters", master, "versions" ]
                                  []
                                  "GET"