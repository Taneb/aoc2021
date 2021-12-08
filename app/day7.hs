module Main where

part1 :: [Int] -> Int
part1 xs = minimum (map calcScore [minimum xs..maximum xs])
  where
    calcScore :: Int -> Int
    calcScore target = sum [abs (x - target) | x <- xs]


part2 :: [Int] -> Int
part2 xs = minimum (map calcScore [minimum xs..maximum xs])
  where
    t :: Int -> Int
    t n = n * (n + 1) `div` 2
    calcScore :: Int -> Int
    calcScore target = sum [t (abs (x - target)) | x <- xs]

main :: IO ()
main = do
  input <- map read . words . map (\x -> if x == ',' then ' ' else x) <$> getContents
  print $ part1 input
  print $ part2 input