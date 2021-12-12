module Main where

import Data.Char (isLower)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type CaveSystem = Map String (Set String)
type Explorations = Map (String, Set String) Int

parse :: [String] -> CaveSystem
parse [] = M.empty
parse (l:ls) = M.insertWith S.union source (S.singleton target) $ M.insertWith S.union target (S.singleton source) $ parse ls
  where
    source, target :: String
    (source, '-':target) = break (== '-') l

naive :: CaveSystem -> String -> Set String -> Int
naive _ "end" _ = 1
naive system here visited =
  let next = M.findWithDefault S.empty here system `S.difference` visited
  in foldr (\n t -> t + naive system n (if isLower (head n) then S.insert n visited else visited)) 0 next

part1 :: CaveSystem -> Int
part1 system = naive system "start" (S.singleton "start")

naive2 :: CaveSystem -> String -> Set String -> Int
naive2 _ "end" _ = 1
naive2 system here visited =
  let next = M.findWithDefault S.empty here system `S.difference` S.singleton "start"
  in foldr (\n t -> t + if n `S.member` visited then naive system n visited else naive2 system n (if isLower (head n) then S.insert n visited else visited)) 0 next

part2 :: CaveSystem -> Int
part2 system = naive2 system "start" (S.singleton "start")

main :: IO ()
main = do
  system <- parse . lines <$> getContents 
  print $ part1 system
  print $ part2 system