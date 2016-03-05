module Discogs
  (runDiscogs
  , runDiscogsAnon
  , runDiscogsWith
  , runResumeDiscogsWith
  , interpretIO
  , DiscogsOptions(..)
  , defaultDiscogsOptions
  , LoginMethod(..)
  , APIError(..)
  , module Discogs.Types.Error
  , module Discogs.Types.Discogs) where

import Discogs.Actions()
import Discogs.Types()
import Discogs.Login
import Discogs.Types.Error
import Discogs.Types.Discogs

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Builder as API
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

data DiscogsOptions =
  DiscogsOptions { rateLimitingEnabled :: Bool
                , connectionManager :: Maybe Manager
                , loginMethod :: LoginMethod
                , customUserAgent :: Maybe ByteString }

instance Default DiscogsOptions where
  def = DiscogsOptions True Nothing Anonymous Nothing

-- | The default set of options
defaultDiscogsOptions :: DiscogsOptions
defaultDiscogsOptions = def

-- | Should we log in to Discogs? If so, should we use a stored set of credentials
--   or get a new fresh set?
data LoginMethod = Anonymous -- ^ Don't login, instead use an anonymous account
                 | Credentials Text Text -- ^ Login using the specified username and password
                 | StoredDetails LoginDetails -- ^
                 --   Login using a stored set of credentials. Usually the best way to get
                 --   these is to do @'runDiscogsAnon' $ 'login' user pass@.
  deriving (Show)

instance Default LoginMethod where def = Anonymous

-- | Run a 'Discogs' action (or a 'DiscogsT' transformer action). This uses the default logged-in settings
--   for 'DiscogsOptions': rate limiting enabled, default manager, login via username and password, and
--   the default user-agent. You should change the user agent if you're making anything more complex than
--   a basic script, since Discogs's API policy says that you should have a uniquely identifiable user agent.
runDiscogs :: MonadIO m => Text -> Text -> DiscogsT m a -> m (Either (APIError DiscogsError) a)
runDiscogs user pass = runDiscogsWith def { loginMethod = Credentials user pass }

-- | Run a 'Discogs' action (or a 'DiscogsT' transformer action). This uses the default logged-out settings, so
--   you won't be able to do anything that requires authentication (like checking messages or making a post).
--   At the moment, authentication isn't statically checked, so it'll return a runtime error if you try to do
--   anything you don't have permissions for.
runDiscogsAnon :: MonadIO m => DiscogsT m a -> m (Either (APIError DiscogsError) a)
runDiscogsAnon = runDiscogsWith def

-- | Run a 'Discogs' or 'DiscogsT' action with custom settings. You probably won't need this function for
--   most things, but it's handy if you want to persist a connection over multiple 'Discogs' sessions or
--   use a custom user agent string.
runDiscogsWith :: MonadIO m => DiscogsOptions -> DiscogsT m a -> m (Either (APIError DiscogsError) a)
runDiscogsWith opts discogs = liftM dropResume $ runResumeDiscogsWith opts discogs

-- | Run a 'Discogs' or 'DiscogsT' action with custom settings. You probably won't need this function for
--   most things, but it's handy if you want to persist a connection over multiple 'Discogs' sessions or
--   use a custom user agent string.
runResumeDiscogsWith :: MonadIO m => DiscogsOptions -> DiscogsT m a -> m (Either (APIError DiscogsError, Maybe (DiscogsT m a)) a)
runResumeDiscogsWith (DiscogsOptions rl man lm _ua) reddit = do
  manager <- case man of
    Just m -> return m
    Nothing -> liftIO $ newManager tlsManagerSettings
  loginCreds <- case lm of
    Anonymous -> return $ Right Nothing
    StoredDetails ld -> return $ Right $ Just ld
    Credentials user pass -> liftM (fmap Just) $ interpretIO (DiscogsState loginBaseURL rl manager [] Nothing) $ login user pass
  case loginCreds of
    Left (err, _) -> return $ Left (err, Just reddit)
    Right lds ->
      interpretIO
        (DiscogsState mainBaseURL rl manager [("User-Agent", "reddit-haskell dev version")] lds) reddit

interpretIO :: MonadIO m => DiscogsState -> DiscogsT m a -> m (Either (APIError DiscogsError, Maybe (DiscogsT m a)) a)
interpretIO rstate (DiscogsT r) =
  runFreeT r >>= \case
    Pure x -> return $ Right x
    Free (WithBaseURL u x n) ->
      interpretIO (rstate { currentBaseURL = u }) x >>= \case
        Left (err, Just resume) ->
          return $ Left (err, Just $ resume >>= DiscogsT . n)
        Left (err, Nothing) -> return $ Left (err, Nothing)
        Right res -> interpretIO rstate $ DiscogsT $ n res
    Free (FailWith x) -> return $ Left (x, Nothing)
    Free (Nest x n) ->
      interpretIO rstate $ DiscogsT $ wrap $ NestResuming x (n . dropResume)
    Free (NestResuming x n) -> do
      res <- interpretIO rstate x
      interpretIO rstate $ DiscogsT $ n res
    Free (RunRoute route n) ->
      interpretIO rstate $ DiscogsT $ wrap $ ReceiveRoute route (n . unwrapJSON)
    Free (ReceiveRoute route n) ->
      handleReceive route rstate >>= \case
        Left err@(APIError (RateLimitError secs _)) ->
          if rateLimit rstate
            then do
              liftIO $ threadDelay $ fromInteger secs * 1000 * 1000
              interpretIO rstate $ DiscogsT $ wrap $ ReceiveRoute route n
            else return $ Left (err, Just $ DiscogsT $ wrap $ ReceiveRoute route n)
        Left err -> return $ Left (err, Just $ DiscogsT $ wrap $ ReceiveRoute route n)
        Right x -> interpretIO rstate $ DiscogsT $ n x

dropResume :: Either (APIError DiscogsError, Maybe (DiscogsT m a)) a -> Either (APIError DiscogsError) a
dropResume (Left (x, _)) = Left x
dropResume (Right x) = Right x

handleReceive :: (MonadIO m, Receivable a) => Route -> DiscogsState -> m (Either (APIError DiscogsError) a)
handleReceive d dstate = do
  (res, _, _) <- runAPI (builderFromState dstate) (connMgr dstate) () $
    API.runRoute d
  return res

builderFromState :: DiscogsState -> Builder
builderFromState (DiscogsState burl _ _ hdrs (Just (LoginDetails (Modhash mh) cj))) =
  Builder "Discogs" burl addAPIType $
    \req -> addHeaders (("X-Modhash", encodeUtf8 mh):hdrs) req { cookieJar = Just cj }
builderFromState (DiscogsState burl _ _ hdrs Nothing) =
  Builder "Discogs" burl addAPIType (addHeaders hdrs)

addHeaders :: [Header] -> Request -> Request
addHeaders xs req = req { requestHeaders = requestHeaders req ++ xs }

data DiscogsState =
  DiscogsState { currentBaseURL :: Text
              , rateLimit :: Bool
              , connMgr :: Manager
              , _extraHeaders :: [Header]
              , _creds :: Maybe LoginDetails }