-- | This module contains all actions that wrap Discog's API endpoints.
-- Only database actions (with the exception of search) have been implemented so far.
module Discogs.Actions
  ( module Discogs.Actions.Artist
  , module Discogs.Actions.Release
  , module Discogs.Actions.Master
  , module Discogs.Actions.Label ) where

import Discogs.Actions.Artist  
import Discogs.Actions.Release
import Discogs.Actions.Master
import Discogs.Actions.Label