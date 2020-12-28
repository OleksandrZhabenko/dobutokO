-- |
-- Module      :  System.Extended.MathPauses.PrintMessage
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Creates the time intervals for CLI changing messages on the screen.
--

{-# OPTIONS_GHC -threaded #-}

module System.Extended.MathPauses.PrintMessage (
  -- * IO functions
  printUpdatedMsg
  , printUpdatedMsg2
  -- * Pure funcions
  -- ** No \'divine proportion\' inside
  , endZQuot
  , endZQuotInteger
  -- ** The \'divine proportion\' inside
  , phi0
  , timeIntervalsPHi0
  , timeIntervalsPHi0s
  -- *** Differences functions
  , deltaTimePHi0
  , deltaTimePHi0s
) where

import Data.Tuple (swap)
import Control.Exception (Exception, catch, throwIO)
import Control.Concurrent (threadDelay)
import System.IO

-- | Function 'printUpdatedMsg' prints a sequence of the first @String@ concatenated with the appropriate element of the second @[String]@ argument
-- through special time interval specified by the @Int@ argument after the pause in the @Int@ microseconds (1/10^6 second)
-- (approximately, depending on the GHC generated code performance). This looks like continually updated string on the screen of the terminal. All
-- String arguments must not contain the special symbols \'\n\' and \'\r\' and vertical spaces. Please, check by yourself that the @Int@
-- argument does not exceed the @maxBound :: Int@.
printUpdatedMsg :: String -> [String] -> Int -> IO ()
printUpdatedMsg xs (zs:zss) y = do
  putStr $ xs ++ zs
  threadDelay y
  hFlush stdout
  putStr "\r"
  printUpdatedMsg xs zss y
printUpdatedMsg xs [] y = putStr ""

-- | Function 'printMessage' works the same as the function 'printUpdatedMsg' with the difference that time intervals are specified by the list of @Int@s.
-- The length of the lists must be equal, otherwise the function will truncate them to the less of the both ones length. Please, check by yourself that every @Int@
-- argument does not exceed the @maxBound :: Int@.
printUpdatedMsg2 :: String -> [String] -> [Int] -> IO ()
printUpdatedMsg2 xs (zs:zss) (t:ts) = do
  putStr $ xs ++ zs
  threadDelay t
  hFlush stdout
  putStr "\r"
  printUpdatedMsg2 xs zss ts
printUpdatedMsg2 xs [] _ = putStr ""  
printUpdatedMsg2 xs _ [] = putStr ""

-- | Function 'endZQuot' returns a tuple of numbers -- a fractional part and a whole part of the number. 
-- Inspired by: 'https://mail.haskell.org/pipermail/beginners/2009-November/002814.html'
endZQuot :: Double -> (Double, Integer)
endZQuot x = swap . properFraction $ x

-- | Function 'endZQuotInteger' splits a @Double@ argument to a less by absolute value @Double@ specified by the @Integer@ argument with some
-- regularization for the not natural values to avoid the undefined situations.
endZQuotInteger :: Double -> Integer -> IO (Double, Integer)
endZQuotInteger x z | compare (fromIntegral z) x == LT && compare z 0 /= LT = return . (\(t, u) w -> (t + fromIntegral w, u - fromIntegral w)) (endZQuot x) $ z
                    | otherwise = endZQuotInteger (abs x) (abs z `quot` 3)

-- | Famous mathematical constant of the 1/(\'golden ratio\').
phi0 :: Double
phi0 = (sqrt 5 - fromIntegral 1) / 2.0

-- | Function 'timeIntervalPHi0' produces a list of @Int@s of the time interval values for the 'threadDelay' function from the 'System.IO' module.
-- These time interval points can create for some users an impression of the smooth convergence to the end of the time intervals.
timeIntervalsPHi0 :: Double -> [Int]
timeIntervalsPHi0 x | compare x 1 == GT = takeWhile (\t1 -> t1 > 1000000) . map (\t2 -> truncate . (* t2) . fromIntegral $ 1000000) . iterate (\t0 -> t0 * phi0) $ x
                    | otherwise = [1000000]

-- | The same as 'timeIntervalsPHi0' function, but the result is in seconds.
timeIntervalsPHi0s :: Double -> [Double]
timeIntervalsPHi0s x = map (\t3 -> fromIntegral t3 / 1000000) . timeIntervalsPHi0 $ x

-- | Function 'deltaTimePHi0' returns a list of @Int@s of the differences between the neighbour values in the list produced by the 'timeIntervalsPHi0' function.
deltaTimePHi0 :: Double -> [Int]
deltaTimePHi0 x | compare x 1 == GT = zipWith subtract (tail . timeIntervalsPHi0 $ x) (timeIntervalsPHi0 x) 
                | otherwise = [0 :: Int]

-- | The same as 'deltaTimePHi0' function, but the result is in seconds.
deltaTimePHi0s :: Double -> [Double]
deltaTimePHi0s x = map (\t3 -> fromIntegral t3 / 1000000) . deltaTimePHi0 $ x
