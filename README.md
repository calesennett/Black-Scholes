Black-Scholes-Merton Model
=============

Implementation of the Black-Scholes-Merton model for pricing European options in Haskell.

Use
------------
    ghci
    :l BlackScholes.hs
#### For a call option 
    blackScholes Option { optionType="call", stock=31.25, strike=22.75, riskFree=0.01, time=1, vol=0.5 }
#### For a put option 
    blackScholes Option { optionType="put", stock=31.25, strike=35.00, riskFree=0.01, time=1, vol=0.5 }
####To run tests:
    cabal install HSpec
    runhaskell Tests.hs
