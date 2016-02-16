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
                 | RateLimitError Integer Text
                 | InvalidResponseError
                 deriving (Show, Eq)

instance FromJSON DiscogsError where
  parseJSON (Object o) = do
    Array errors <- o .: "json" >>= (.: "errors")
    case errors !? 0 of
      Just (Array e) -> case V.toList e of
        --String "WRONG_PASSWORD" : _ -> return CredentialsError
        --String "USER_REQUIRED" : _ -> return CredentialsError
        String "RATELIMIT" : String d : _ ->
            RateLimitError <$> ((o .: "json") >>= (.: "ratelimit")) <*> pure d
        --String "SUBREDDIT_REQUIRED" : _ -> return NoSubredditSpecified
        --String "ALREADY_SUB" : _ -> return AlreadySubmitted
        --String "NO_URL" : _ -> return NoURLSpecified
        --String "NO_NAME" : _ -> return NoName
        --String "NO_TEXT" : _ : String f : _ -> return $ NoText f
        --String "COMMENT_DELETED" : _ -> return CommentDeleted
        --String "DELETED_LINK" : _ -> return LinkDeleted
        --String "BAD_SR_NAME" : _ -> return BadSubredditName
        --String "BAD_CAPTCHA" : _ -> CaptchaError <$> (o .: "json" >>= (.: "captcha"))
        _ -> return $ DiscogsError o
      _ -> mempty
  parseJSON _ = mempty

instance ErrorReceivable DiscogsError where
  receiveError = useErrorFromJSON