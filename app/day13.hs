module Main where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S

parsePoints :: [String] -> Set (Int, Int)
parsePoints = S.fromList . map parsePoint
  where
    parsePoint :: String -> (Int, Int)
    parsePoint r = case break (== ',') r of
      (x, ',':y) -> (read x, read y)
      _ -> error $ "Unrecognized point: " ++ r


parseFolds :: [String] -> [Either Int Int]
parseFolds = map (parseFold . drop 11)
  where
    parseFold ('x':'=':r) = Left (read r)
    parseFold ('y':'=':r) = Right (read r)
    parseFold r = error $ "unrecognized fold: " ++ r

parse :: [String] -> (Set (Int, Int), [Either Int Int])
parse ls = case break null ls of
  (ps, "":fs) -> (parsePoints ps, parseFolds fs)
  _ -> error "Couldn't parse"

-- fold x around p: x' = p - (x - p) = 2 * p - x

foldSheet :: Set (Int, Int) -> Either Int Int -> Set (Int, Int)
foldSheet ps f = S.map foldPoint ps
  where
    foldPoint :: (Int, Int) -> (Int, Int)
    foldPoint (x, y)
      | either (< x) (< y) f = either (\p -> (2 * p - x, y)) (\p -> (x, 2 * p - y)) f
      | otherwise = (x, y)

part1 :: Set (Int, Int) -> [Either Int Int] -> Int
part1 ps (f:fs) = S.size $ foldSheet ps f
part1 _ _ = error "Needs at least one fold"

draw :: Set (Int, Int) -> IO ()
draw ps = forM_ [0..14] $ \j -> do
  forM_ [0..79] $ \i -> putChar $ if (i, j) `S.member` ps then '#' else ' '
  putChar '\n'

part2 :: Set (Int, Int) -> [Either Int Int] -> IO ()
part2 ps fs = draw $ foldl foldSheet ps fs

main :: IO ()
main = do
  input <- parse . lines <$> getContents 
  print $ uncurry part1 input
  uncurry part2 input