import System.Environment (getArgs)
import System.Random
import Data.Int
import Data.List

sqr x = x * x

inCircle (x, y) = sqr x + sqr y <= 1

toPairs []             = []
toPairs (x1 : x2 : xs) = (x1, x2) : toPairs xs

monteCarloCircleArea :: Int64 -> [(Double, Double)] -> Double
monteCarloCircleArea num darts = 
  4 * (fromIntegral (length $ filter inCircle $ genericTake num darts)) / (fromIntegral num)

main :: IO ()
main = do
  args <- getArgs
  case map (read :: String -> Int64) args of
    [num] -> do
      g <- getStdGen
      let pi = monteCarloCircleArea num $ toPairs $ (randoms g :: [Double])
      putStrLn $ "pi = " ++ show pi ++ " for " ++ show num ++ " darts"
    _ -> 
      putStrLn "usage: ./montecarlo numDarts"
