module Discogs
  () where

import Discogs.Actions.Artist
import Discogs.Types.Artist
--import Discogs.Routes.Artist as Routes
import Network.API.Builder.Error
import Network.API.Builder



discogs:: Builder
discogs = basicBuilder "Discogs API" "https://api.discogs.com"

--getArtist :: IO (Either (APIError ()) Artist)
--getAnswers = execAPI discogs () $ runRoute Routes.getArtist