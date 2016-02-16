module Discogs.Types.Discogs
  ( Discogs
  , DiscogsT(..)
  , DiscogsF(..)
  , runRoute
  , receiveRoute
  , nest
  , failWith
  , Modhash(..)
  , LoginDetails(..)
  , withBaseURL
  , builder
  , mainBaseURL
  , loginBaseURL
  , addHeader
  , addAPIType ) where

import Discogs.Types.Error

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.API.Builder hiding (runRoute)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types
import Prelude
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS

type Discogs a = DiscogsT IO a

data DiscogsF m a where
  FailWith :: APIError DiscogsError -> DiscogsF m a
  Nest :: DiscogsT m b -> (Either (APIError DiscogsError) b -> a) -> DiscogsF m a
  NestResuming :: DiscogsT m b -> (Either (APIError DiscogsError, Maybe (DiscogsT m b)) b -> a) -> DiscogsF m a
  ReceiveRoute :: Receivable b => Route -> (b -> a) -> DiscogsF m a
  RunRoute :: FromJSON b => Route -> (b -> a) -> DiscogsF m a
  WithBaseURL :: Text -> DiscogsT m b -> (b -> a) -> DiscogsF m a

instance Functor (DiscogsF m) where
  fmap _ (FailWith x) = FailWith x
  fmap f (Nest a x) = Nest a (fmap f x)
  fmap f (NestResuming a x) = NestResuming a (fmap f x)
  fmap f (ReceiveRoute r x) = ReceiveRoute r (fmap f x)
  fmap f (RunRoute r x) = RunRoute r (fmap f x)
  fmap f (WithBaseURL u a x) = WithBaseURL u a (fmap f x)

newtype DiscogsT m a = DiscogsT (FreeT (DiscogsF m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans DiscogsT where
  lift = DiscogsT . lift

instance MonadIO m => MonadIO (DiscogsT m) where
  liftIO = DiscogsT . liftIO

runRoute :: (FromJSON a, Monad m) => Route -> DiscogsT m a
runRoute r = DiscogsT $ liftF $ RunRoute r id

receiveRoute :: (Receivable a, Monad m) => Route -> DiscogsT m a
receiveRoute r = DiscogsT $ liftF $ ReceiveRoute r id

nest :: Monad m => DiscogsT m a -> DiscogsT m (Either (APIError DiscogsError) a)
nest f = DiscogsT $ liftF $ Nest f id

withBaseURL :: Monad m => Text -> DiscogsT m a -> DiscogsT m a
withBaseURL u f = DiscogsT $ liftF $ WithBaseURL u f id

failWith :: Monad m => APIError DiscogsError -> DiscogsT m a
failWith = DiscogsT . liftF . FailWith

newtype Modhash = Modhash Text
  deriving (Show, Read, Eq)

instance FromJSON Modhash where
  parseJSON (Object o) =
    Modhash <$> ((o .: "json") >>= (.: "data") >>= (.: "modhash"))
  parseJSON _ = mempty

data LoginDetails = LoginDetails Modhash CookieJar
  deriving (Show, Eq)

instance Receivable LoginDetails where
  receive x = do
    (resp, mh) <- receive x
    return $ LoginDetails (unwrapJSON mh) (responseCookieJar (resp :: Response ByteString))

newtype POSTWrapped a = POSTWrapped a
  deriving (Show, Read, Eq)

instance Functor POSTWrapped where
  fmap f (POSTWrapped a) = POSTWrapped (f a)

builder :: Builder
builder = Builder "Discogs API"
                  mainBaseURL
                  addAPIType
                  (addHeader Nothing)

addHeader :: Maybe BS.ByteString -> Request -> Request
addHeader Nothing req = req { requestHeaders =
  ("User-Agent", "discogs-haskell 0.1.0.0 / accraze") : requestHeaders req }
addHeader (Just hdr) req = req { requestHeaders =
  ("User-Agent", hdr) : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ps m

mainBaseURL :: Text
mainBaseURL = "https://api.discogs.com"

loginBaseURL :: Text
loginBaseURL = "https://api.discogs.com"