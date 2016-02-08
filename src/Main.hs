module Main where

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Network.Wreq


main :: IO ()
main = do
  response <- get "https://api.discogs.com/releases/249504"
  print $ BL.take 1000 (response ^. responseBody)