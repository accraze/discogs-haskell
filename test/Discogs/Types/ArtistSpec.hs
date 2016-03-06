module Discogs.Types.ArtistSpec where

import Discogs.Types.Artist

import Data.Aeson (eitherDecode)
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Discogs.Types.Artist" $ do
    getArtistExample <- runIO $ ByteString.readFile "test/data/getArtist_example.json"
    getArtistReleasesExample <- runIO $ ByteString.readFile "test/data/getArtistReleases_example.json"

    it "can read the fixtures" $ do
      getArtistExample `shouldSatisfy` not . ByteString.null
      getArtistReleasesExample `shouldSatisfy` not . ByteString.null

    it "can parse an artist from json" $ do
      let decoded = eitherDecode getArtistExample :: Either String Artist
      decoded `shouldSatisfy` isRight

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right artist -> do
          artistID artist `shouldBe` ArtistID 108713
          releases_url artist `shouldBe` "https://api.discogs.com/artists/108713/releases"
          resource_url artist `shouldBe` Just "https://api.discogs.com/artists/108713"
          uri artist `shouldBe` Just "https://www.discogs.com/artist/108713-Nickelback"
          data_quality artist `shouldBe` "Needs Vote"
          namevariations artist `shouldBe`  Just ["Nickleback","\12491\12483\12465\12523\12496\12483\12463"]