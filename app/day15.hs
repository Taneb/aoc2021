module Main where

import Data.Array
import Data.Char
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Set (Set)
import qualified Data.Set as S

parseInput :: [String] -> Array (Int, Int) Int
parseInput xs =
  let
    height = length xs
    width  = length (head xs)
  in listArray ((1,1),(width, height)) $ map digitToInt $ concat xs

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

dijkstra' :: Array (Int, Int) Int -> Set (Int, Int) -> MinPQueue Int (Int, Int) -> Int
dijkstra' graph visited dists = case PQ.minViewWithKey dists of
  Nothing -> error "Destination was unreachable"
  Just ((dist, here), dists')
    | here == snd (bounds graph) -> dist
    | here `S.member` visited -> dijkstra' graph visited dists'
    | otherwise ->
        let
          adjs = filter (bounds graph `inRange`) $ adjacents here
          dists'' = foldr (\adj -> PQ.insert (dist + graph ! adj) adj) dists' adjs
        in dijkstra' graph (S.insert here visited) dists''

dijkstra :: Array (Int, Int) Int -> Int
dijkstra graph = dijkstra' graph S.empty $ PQ.singleton 0 (1, 1)

part1 :: Array (Int, Int) Int -> Int
part1 = dijkstra

extendGraph :: Array (Int, Int) Int -> Array (Int, Int) Int
extendGraph graph =
  let
    (_, (w, h)) = bounds graph
    newValue (x, y) =
      let
        (xm, x') = (x - 1) `divMod` w
        (ym, y') = (y - 1) `divMod` h
        nb = (x' + 1, y' + 1)
        m = xm + ym
        d = graph ! nb
        d' = (d + m - 1) `mod` 9 + 1
       in d'
    
    newBounds = (w * 5, h * 5)
  in array ((1, 1), newBounds) [(c, newValue c) | c <- range ((1, 1), newBounds) ]

part2 :: Array (Int, Int) Int -> Int
part2 = dijkstra . extendGraph

main :: IO ()
main = do
  graph <- parseInput . lines <$> getContents
  print $ part1 graph
  print $ part2 graph