module Test.BlackScholes where

import qualified BlackScholes as BS
import           Test.Hspec

option1 = BS.Option {BS.optionType="call", BS.stock=31.25, BS.strike=22.75, BS.rate=0.01, BS.time=1, BS.vol=0.5}
option2 = BS.Option {BS.optionType="put", BS.stock=31.25, BS.strike=35.00, BS.rate=0.01, BS.time=1, BS.vol=0.5}

main = hspec $ do
    describe "normcdf" $ do
        it "returns correct value" $ do
            BS.normcdf 2 `shouldSatisfy` within 0.001 0.97724987

    describe "blackScholes" $ do
        it "uses call function for a call option" $ do
            let d1 = ((log (BS.stock o / BS.strike o)) + ((BS.rate o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option1
            BS.blackScholes option1 `shouldBe` BS.bsCall option1 d1 d2

        it "uses put function for a put option" $ do
            let d1 = ((log (BS.stock o / BS.strike o)) + ((BS.rate o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option2
            BS.blackScholes option2 `shouldBe` BS.bsPut option2 d1 d2

        it "can compute value of a call option" $ do
            let d1 = ((log (BS.stock o / BS.strike o)) + ((BS.rate o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option1
            BS.blackScholes option1 `shouldSatisfy` within 0.001 10.736

        it "can compute value a put option" $ do
            let d1 = ((log (BS.stock o / BS.strike o)) + ((BS.rate o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option2
            BS.blackScholes option2 `shouldSatisfy` within 0.001 8.3412

within
    :: Double
    -> Double
    -> Double
    -> Bool
within
    tolerance
    expected
    actual = if ((expected - tolerance) < actual) && (actual < (expected + tolerance))
             then True
             else False
