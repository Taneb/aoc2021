module Main where

import Data.List
import Data.Maybe

part1 :: [[String]] -> Int
part1 = sum . map f1
  where
    f1 = f2 . tail . dropWhile (/= "|")

    f2 = length . filter (\x -> length x `elem` [2,3,4,7])

part2 :: [[String]] -> Int
part2 = sum . map f1
  where
    f1 xs = 
      let 
        (signal, "|":digits) = break (== "|") xs
        (d1:d7:d4:d23or5a:d23or5b:d23or5c:d06or9a:d06or9b:d06or9c:d8:[]) = map sort . sortOn length $ signal
        (d3:[], d2or5a:d2or5b:[]) = partition (d1 `isSubsequenceOf`) [d23or5a, d23or5b, d23or5c]
        (d9:[], d0or6a:d0or6b:[]) = partition (d4 `isSubsequenceOf`) [d06or9a, d06or9b, d06or9c]
        (d0:[], d6:[]) = partition (d1 `isSubsequenceOf`) [d0or6a, d0or6b]
        (d5:[], d2:[]) = partition (`isSubsequenceOf` d6) [d2or5a, d2or5b]
      in foldl (\r a -> 10 * r + a) 0 $ mapMaybe (\d -> elemIndex (sort d) [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9]) digits

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  print $ part1 input
  print $ part2 input
