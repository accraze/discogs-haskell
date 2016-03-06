module Discogs.Types.ReleaseSpec where

import Discogs.Types.Release

import Data.Aeson (eitherDecode)
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Discogs.Types.Release" $ do
    getReleaseExample <- runIO $ ByteString.readFile "test/data/getRelease_example.json"

    it "can read the fixtures" $ do
      getReleaseExample `shouldSatisfy` not . ByteString.null

    it "can parse a release from json" $ do
      let decoded = eitherDecode getArtistExample :: Either String Release
      decoded `shouldSatisfy` isRight

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right artist -> do
          title release `shouldBe` "Never Gonna Give You Up"
          id release `shouldBe` 249504
          resource_url release `shouldBe` Just "https://api.discogs.com/artists/72872"
          country release `shouldBe` Just "UK"
          estimated_weight release `shouldBe` 60
          date_added release `shouldBe`  "2004-04-30T08:10:05-07:00"
          date_changed release `shouldBe`  "2012-12-03T02:50:12-07:00"
          year release `shouldBe`  1987