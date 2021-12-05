module Main where

import Data.List
import Data.Function
import Data.Char

part1 :: [String] -> Int
part1 = uncurry (*) . foldl bin (0, 0) . map (figure . group . sort) . transpose
  where
    figure :: [String] -> Char
    figure = fst . maximumBy (compare `on` snd) . map (\xs -> (head xs, length xs))

    bin (r, r') x = (2 * r + digitToInt x, 2 * r' + 1 - digitToInt x)

part2 :: [String] -> Int
part2 xs = foldl bin 0 (go xs) * foldl bin 0 (go' xs)
  where
    go :: [String] -> String
    go [x] = x
    go xs = let mostCommonDigit = fst . maximumBy (compare `on` snd) . map (\xs -> (head xs, length xs)) . group . sort . map head $ xs
            in mostCommonDigit : go (map tail $ filter (\p -> head p == mostCommonDigit) xs)

    go' :: [String] -> String
    go' [x] = x
    go' xs = let mostCommonDigit = fst . minimumBy (compare `on` snd) . map (\xs -> (head xs, length xs)) . group . sort . map head  $ xs
            in mostCommonDigit : go' (map tail $ filter (\p -> head p == mostCommonDigit) xs)

    bin r x = digitToInt x + 2 * r

main = do
  input <- lines <$> getContents
  print $ part1 input
  print $ part2 input
