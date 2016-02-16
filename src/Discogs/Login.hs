module Discogs.Login
  ( login ) where

import Discogs.Types.Discogs

import Data.Text (Text)
import Network.API.Builder hiding (runRoute)

loginRoute :: Text -> Text -> Route
loginRoute user pass = Route [ "api", "login" ]
                             [ "rem" =. True
                             , "user" =. user
                             , "passwd" =. pass ]
                             "POST"

getLoginDetails :: Monad m => Text -> Text -> DiscogsT m LoginDetails
getLoginDetails user pass = receiveRoute $ loginRoute user pass

-- | Make a login request with the given username and password.
login :: Monad m
      => Text -- ^ Username to login with
      -> Text -- ^ Password to login with
      -> DiscogsT m LoginDetails
login = getLoginDetails