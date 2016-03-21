module Discogs.Types.Error
  (DiscogsError(..)) where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Vector ((!?))
import Network.API.Builder.Receive
import Prelude
import qualified Data.Vector as V

data DiscogsError = DiscogsError Object
                 | NoRelease
                 | NoArtist
                 | RateLimitError Integer Text
                 | NoMasterRelease
                 | NoLabel
                 deriving (Show, Eq)

instance FromJSON DiscogsError where
  parseJSON (Object o) = do
    Array errors <- o .: "json" >>= (.: "message")
    case errors !? 0 of
      Just (Array e) -> case V.toList e of
        String "Release not found." : _ -> return NoRelease
        String "Artist not found." : _ -> return NoArtist
        String "RATELIMIT" : String d : _ ->
            RateLimitError <$> ((o .: "json") >>= (.: "ratelimit")) <*> pure d
        String "Master Release not found." : _ -> return NoMasterRelease
        String "Label not found." : _ -> return NoLabel
        _ -> return $ DiscogsError o
      _ -> mempty
  parseJSON _ = mempty

instance ErrorReceivable DiscogsError where
  receiveError = useErrorFromJSON