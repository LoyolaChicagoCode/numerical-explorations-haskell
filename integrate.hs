import Data.List

square :: Double -> Double
square x = x * x

integrate :: Double -> Double -> Integer -> (Double -> Double) -> Double
integrate a b n f =
    let interval = (b - a) / (fromIntegral n)
        f' x = f (a + x * interval)
        fxValues = map f' [(fromIntegral 1)..(fromIntegral (n-1))] 
    in foldl' (+) 0.0 fxValues

main = print (integrate 0 100 100000000 square)
