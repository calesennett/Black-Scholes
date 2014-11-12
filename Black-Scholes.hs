module BlackScholes where

{- Cumulative Standard Normal Distribution
 - Credit to: Karl M. Syring
 -}
normcdf x
    | x < 0     = 1 - w
    | otherwise = w
        where
            w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * poly k
            k = 1.0 / (1.0 + 0.2316419 * l)
            l = abs x
            poly = horner coeff
            coeff = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]

horner coeff base = foldr1 multAdd coeff
    where
        multAdd x y = y * base + x
