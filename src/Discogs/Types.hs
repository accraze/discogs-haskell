-- | This module contains all types of data returned by the Discogs API.
-- Only database actions (with the exception of search) have been implemented so far.
module Discogs.Types ( Artist, Release, Master, Label ) where

import Discogs.Types.Artist (Artist)
import Discogs.Types.Release (Release)
import Discogs.Types.Master (Master)
import Discogs.Types.Label (Label)