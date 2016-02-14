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
                 | FailError Text
                 | InvalidResponseError
                 deriving (Show, Eq)