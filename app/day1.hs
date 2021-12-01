module Main where

part1 :: [Int] -> Int
part1 (a:b:r) | a < b = 1 + part1 (b:r)
              | otherwise = part1 (b:r)
part1 _ = 0

part2 :: [Int] -> Int
part2 (a:b:c:d:r) | a < d = 1 + part2 (b:c:d:r)
                  | otherwise = part2 (b:c:d:r)
part2 _ = 0

main :: IO ()
main = do
  input <- fmap (map read . lines) getContents
  print $ part1 input
  print $ part2 input

