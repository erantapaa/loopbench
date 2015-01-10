{-# LANGUAGE BangPatterns #-}

import System.Environment

type Number = Int

{-# INLINE isp' #-}
isp' :: Number -> Bool
isp' n' = loop 3
  where ul = floor $ ((sqrt (fromIntegral n)) :: Double)
        loop d | d > ul       = True
               | rem n d == 0 = False
               | otherwise    = loop (d+2)
        n = if even n' then 4194304-3 else 4194304-17 :: Number

-- pi(n) - the prime counting function, the number of prime numbers <= n
primesNo :: Number -> Number
primesNo n = loop 0 n
  where loop !s n | n < 0 = s
        loop !s n = if isp' n then loop (s+1) (n-1) else loop s (n-1)

main = do
  args <- getArgs
  let e :: Int
      e = case args of
            [] -> 22
            (x:_) -> read x
      n = 2^e
  putStrLn $ "pi " ++ show n ++ " = " ++ show (primesNo n)
