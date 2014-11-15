module Test.BlackScholes where

import qualified BlackScholes as BS
import           Test.Hspec


main = hspec $ do
    describe "normcdf" $ do
        it "returns correct value" $ do
            BS.normcdf 2 `shouldSatisfy` within 0.001 0.97724987

    describe "blackScholes" $ do
        it "uses call function for a call option" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=22.75
                                   , BS.riskFree=0.01
                                   , BS.time=1
                                   , BS.vol=0.5
                                   }
                d1 = ((log (BS.stock o / BS.strike o)) + ((BS.riskFree o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option
            BS.blackScholes option `shouldBe` BS.bsCall option d1 d2

        it "uses put function for a put option" $ do
            let option = BS.Option { BS.optionType="put"
                                   , BS.stock=31.25
                                   , BS.strike=35.00
                                   , BS.riskFree=0.01
                                   , BS.time=1
                                   , BS.vol=0.5
                                   }
                d1 = ((log (BS.stock o / BS.strike o)) + ((BS.riskFree o + (BS.vol o)^2 / 2.0) * (BS.time o))) / ((BS.vol o) * sqrt (BS.time o))
                d2 = d1 - (BS.vol o) * sqrt (BS.time o)
                o  = option
            BS.blackScholes option `shouldBe` BS.bsPut option d1 d2

        it "can compute value of a call option" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=22.75
                                   , BS.riskFree=0.01
                                   , BS.time=1
                                   , BS.vol=0.5
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 10.736

        it "can compute value a put option" $ do
            let option = BS.Option { BS.optionType="put"
                                   , BS.stock=31.25
                                   , BS.strike=35.00
                                   , BS.riskFree=0.01
                                   , BS.time=1
                                   , BS.vol=0.5
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 8.3412

        it "can compute value on expiration date" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=22.75
                                   , BS.riskFree=0.01
                                   , BS.time=0.0
                                   , BS.vol=0.5
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 8.5

        it "can compute value with risk-free rate of 0" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=22.75
                                   , BS.riskFree=0.0
                                   , BS.time=1
                                   , BS.vol=0.5
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 10.5892

        it "can compute value with volatility of 0" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=22.75
                                   , BS.riskFree=0.01
                                   , BS.time=1
                                   , BS.vol=0.0
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 8.7264

        it "can compute a worthless option on expiration date" $ do
            let option = BS.Option { BS.optionType="call"
                                   , BS.stock=31.25
                                   , BS.strike=35.00
                                   , BS.riskFree=0.01
                                   , BS.time=0.0
                                   , BS.vol=0.5
                                   }
            BS.blackScholes option `shouldSatisfy` within 0.001 0

within
    :: Double
    -> Double
    -> Double
    -> Bool
within
    tolerance
    expected
    actual = if ((expected - expected * tolerance) <= actual) && (actual <= (expected + expected * tolerance))
             then True
             else False
