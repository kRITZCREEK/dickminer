module Dickmine.ParseSpec where

import           Test.Hspec

import           Data.Time
import           Dickmine.Parse
import           Dickmine.Types
import           System.Locale

incorrectLogEntry :: [String]
incorrectLogEntry =
  [
    "07/06/2015 08:46:41 am 37.201.171.211 /",
    "Country: (Unknown Country?) (XX)",
    "City: (Unknown City?)",
    "Latitude:",
    "Longitude:",
    "IP: 37.201.171.211"
  ]


correctLogEntry :: [String]
correctLogEntry =
  [
    "Rick Roll Type: Docs",
    "Date: 07/08/2015 04:31:56 am",
    "URL: /api/docs/",
    "Country: IRELAND (IE)",
    "City: Dublin",
    "Latitude: 53.3333",
    "Longitude: -6.25",
    "IP: 137.43.28.224"
  ]

spec = do
  describe "split Date + IP + Route Line" $ do
    it "splits Date from IP from Route" $ do
      splitDateIPRoute "07/06/2015 08:46:41 am 37.201.171.211 /" `shouldBe` ("07/06/2015 08:46:41 am", "37.201.171.211", "/")

  describe "parse Date" $ do
    it "fails to parse a malformed date" $ do
      parseDateString "0213asdjk213" `shouldBe` Nothing
    it "parses a dateString" $ do
      parseDateString "Date: 07/06/2015 08:46:41 am" `shouldBe` Just (readTime defaultTimeLocale "%m/%d/%Y %I:%M:%S %p" "07/06/2015 08:46:41 AM")

  describe "parse IP" $ do
    it "fails to parse a malformed IP" $ do
      parseIP "asdh" `shouldBe` Nothing
    it "parses a correct IP" $ do
      parseIP "IP: 37.201.171.211" `shouldBe` Just "37.201.171.211"

  describe "parse Country" $ do
    it "fails to parse a malformed country String" $ do
      parseCountry "Countasd: (Whatever)" `shouldBe` Nothing
    it "parses a correct Country String" $ do
      parseCountry "Country: (Unknown Country?) (XX)" `shouldBe` Just "(Unknown Country?) (XX)"

  describe "parse City" $ do
    it "fails to parse a malformed city String" $ do
      parseCity "Countasd: (Whatever)" `shouldBe` Nothing
    it "parses a correct city String" $ do
      parseCity "City: (Unknown City?)" `shouldBe` Just "(Unknown City?)"

  describe "parse Latitude" $ do
    it "failes to parse a malformed Latitude" $ do
      parseLatitude "Counasdmh:asdkj" `shouldBe` Nothing
      parseLatitude "Latitude: asd" `shouldBe` Nothing
      parseLatitude "Latitude:" `shouldBe` Nothing
    it "parses a Latitude" $ do
      parseLatitude "Latitude: 42.12" `shouldBe` Just 42.12

  describe "parseLogEntry" $ do
    it "fails to parse on empty Input" $ do
      parseLogEntry [] `shouldBe` Nothing
    it "fails to parse an incorrect Log Entry" $ do
      parseLogEntry incorrectLogEntry `shouldBe` Nothing

    it "parses a correct Log Entry" $ do
      parseLogEntry correctLogEntry `shouldBe`
        Just Pagehit {
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
