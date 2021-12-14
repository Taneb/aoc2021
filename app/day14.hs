module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

countPairs :: [Char] -> Map (Char, Char) Int
countPairs [] = M.empty
countPairs [_] = M.empty
countPairs (a:b:r) = M.insertWith (+) (a, b) 1 $ countPairs (b:r)

stepOnPairCounts :: Map (Char, Char) Char -> Map (Char, Char) Int -> Map (Char, Char) Int
stepOnPairCounts ks = M.foldrWithKey go M.empty
  where
    go :: (Char, Char) -> Int -> Map (Char, Char) Int -> Map (Char, Char) Int
    go p@(a, b) k = case p `M.lookup` ks of
      Nothing -> M.insertWith (+) p k
      Just c -> M.insertWith (+) (a, c) k . M.insertWith (+) (c, b) k

-- NB. undercounts the element that was first in the initial string by 1
pairCountsToCounts :: Map (Char, Char) Int -> Map Char Int
pairCountsToCounts = M.foldrWithKey (\(_, b) -> M.insertWith (+) b) M.empty

score :: Map Char Int -> Int
score = ((-) <$> maximum <*> minimum) . M.elems

parse :: [String] -> (String, Map (Char, Char) Char)
parse (a:"":r) = (a, parseRules r)
  where
    parseRules :: [String] -> Map (Char, Char) Char
    parseRules [] = M.empty
    parseRules (x : xs) = uncurry M.insert (parseRule x) (parseRules xs)

    parseRule :: String -> ((Char, Char), Char)
    parseRule [a, b, ' ', '-', '>', ' ', c] = ((a, b), c)
    parseRule _ = error "Unrecognized rule"
parse _ = error "Unrecognized input"

part1 :: Map (Char, Char) Char -> String -> Int
part1 ks t =
  let
    initial = head t
    ps = countPairs t
  in score . M.adjust (+ 1) initial . pairCountsToCounts $ iterate (stepOnPairCounts ks) ps !! 10


part2 :: Map (Char, Char) Char -> String -> Int
part2 ks t =
  let
    initial = head t
    ps = countPairs t
  in score . M.adjust (+ 1) initial . pairCountsToCounts $ iterate (stepOnPairCounts ks) ps !! 40

main :: IO ()
main = do
  (t, ks) <- parse . lines <$> getContents
  print $ part1 ks t
  print $ part2 ks t