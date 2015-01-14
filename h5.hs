import System.Environment
import qualified Data.Vector.Unboxed as Vector
import Data.Word

foo :: Word -> Bool
foo n = isPrime x
  where
    -- a trick to prevent GHC from optimizing through the if statement
    x = (4194304-3) - (rem n 2)*(17-3)

-- algorithm take from this reddit thread:
--
-- http://www.reddit.com/r/haskell/comments/2s6zu1/haskell_prime_counting_speed_comparison/cnn3gy2

isPrime :: Word -> Bool
isPrime n | n < 2 = False
isPrime 2 = True
isPrime n | n `rem` 2 == 0 = False
isPrime n =
  Vector.all (\i -> n `rem` i /= 0) .
  Vector.enumFromStepN 3 2 $! (floor (sqrt (fromIntegral n) :: Double) - 1) `quot` 2

-- 4194304-3 else 4194304-17

main :: IO ()
main = do
  args <- getArgs
  let e = case args of
            [] -> 22
            (s:_) -> read s
  let n = 2^(e :: Int)
  print . Vector.length . Vector.filter foo . Vector.enumFromN 0 $ n

