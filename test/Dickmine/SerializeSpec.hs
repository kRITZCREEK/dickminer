module Dickmine.SerializeSpec where

import           Data.Time
import           Dickmine.Serialize
import           Dickmine.Types
import           System.Locale
import           Test.Hspec

pageHit =
  Pagehit {
    ip = "137.43.28.224",
    page = "/api/docs/",
    country = "IRELAND (IE)",
    city = "Dublin",
    latitude = 53.3333,
    longitude = -6.25,
    timestamp = readTime defaultTimeLocale
                "%m/%d/%Y %I:%M:%S %p" "07/08/2015 04:31:56 am",
    rrType = "Docs"
    }

serializedPagehit =
  "\"{\\\"country\\\":\\\"IRELAND (IE)\\\",\\\"latitude\\\":53.3333,\\\"page\\\":\\\"/api/docs/\\\",\\\"ip\\\":\\\"137.43.28.224\\\",\\\"city\\\":\\\"Dublin\\\",\\\"rrType\\\":\\\"Docs\\\",\\\"longitude\\\":-6.25,\\\"timestamp\\\":\\\"2015-07-08T04:31:56.000Z\\\"}\""

spec = do
  describe "serialize" $ do
    it "serializes a Pagehit to a JSON String" $ do
     serialize pageHit `shouldBe` serializedPagehit
