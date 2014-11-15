module BlackScholes
    ( Option(..)
    , blackScholes
    , bsCall
    , bsPut
    , normcdf
    ) where

import Data.Char

data Option = Option { optionType :: String
                     , stock      :: Double
                     , strike     :: Double
                     , riskFree   :: Double
                     , time       :: Double
                     , vol        :: Double
                     }

blackScholes
    :: Option
    -> Double
blackScholes
    option
    | (map toLower $ optionType o) == "call" = bsCall option d1 d2
    | otherwise                              = bsPut  option d1 d2
        where   d1 = ((log (stock o / strike o)) + ((riskFree o + (vol o)^2 / 2.0) * (time o))) / ((vol o) * sqrt (time o))
                d2 = d1 - (vol o) * sqrt (time o)
                o  = option

bsCall
    :: Option
    -> Double
    -> Double
    -> Double
bsCall
    o
    d1
    d2 = stock o * (normcdf d1) - strike o * exp (-(riskFree o) * time o) * (normcdf d2)

bsPut
    :: Option
    -> Double
    -> Double
    -> Double
bsPut
    o
    d1
    d2 = strike o * exp (-(riskFree o) * time o) * (normcdf (-d2)) - stock o * (normcdf (-d1))


{- Cumulative Standard Normal Distribution
 - Credit to: Karl M. Syring
 -}
normcdf
    x
    | x < 0     = 1 - w
    | otherwise = w
        where w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * poly k
              k = 1.0 / (1.0 + 0.2316419 * l)
              l = abs x
              poly = horner coeff
              coeff = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]

horner
    coeff
    base = foldr1 multAdd coeff
    where multAdd
              x
              y = y * base + x
