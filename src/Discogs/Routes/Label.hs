module Discogs.Routes.Label where

import Discogs.Types.Label

import Network.API.Builder.Routes

getLabel :: LabelID -> Route
getLabel (LabelID label) = Route [ "labels", label ]
                                  []
                                  "GET"

getLabelReleases :: LabelID -> Route
getLabelReleases (LabelID label) = Route [ "labels", label, "releases" ]
                                  []
                                  "GET"