module Main where

import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Void

-- to compare the sums of two sliding windows, we only need to compare the
-- elements where they differ, that is, the first and last elements.
general :: Monad m => Int -> ConduitT Int o m Int
general windowSize
  =  Conduit.slidingWindow (windowSize + 1)
  .| Conduit.lengthIf measure
  where
    measure :: [Int] -> Bool
    measure xs = head xs < last xs

-- find the answers to both part 1 and part 2
-- we need to use ZipSink here so it sends the input to both parts, without
-- keeping the whole input in memory
solution :: Monad m => ConduitT Int Void m (Int, Int)
solution
  = Conduit.getZipSink $ liftA2 (,)
      (Conduit.ZipSink $ general 1) -- part 1 has a window size of 1
      (Conduit.ZipSink $ general 3) -- part 2 has a window size of 3

-- get the input from stdin
-- it's "correct" but damn inconvenient for stdin to give us a bytestring
getInput :: ConduitT () Int IO ()
getInput
  =  Conduit.stdin
  .| Conduit.linesUnboundedAscii
  .| Conduit.map readAsInt
  where
    readAsInt :: BSC.ByteString -> Int
    readAsInt = BSC.foldl' (\r x -> digitToInt x + 10 * r) 0

main' :: ConduitT () Void IO (Int, Int)
main' = getInput .| solution

main :: IO ()
main = do
  (a, b) <- Conduit.runConduit main'
  print a
  print b
