module Main where

import Data.List (sort)

parseLine :: String -> Either Char String
parseLine = go []
  where
    go xs [] = Right xs

    go xs ('(':bs) = go ('(':xs) bs
    go xs ('[':bs) = go ('[':xs) bs
    go xs ('{':bs) = go ('{':xs) bs
    go xs ('<':bs) = go ('<':xs) bs

    go ('(':xs) (')':bs) = go xs bs
    go ('[':xs) (']':bs) = go xs bs
    go ('{':xs) ('}':bs) = go xs bs
    go ('<':xs) ('>':bs) = go xs bs

    go _ (b:_) = Left b

part1 :: [String] -> Int
part1 = go 0
  where
    go score [] = score
    go score (l:ls) = case parseLine l of
      Right _ -> go score ls
      Left ')' -> go (3 + score) ls
      Left ']' -> go (57 + score) ls
      Left '}' -> go (1197 + score) ls
      Left '>' -> go (25137 + score) ls
      Left _ -> go score ls

part2 :: [String] -> Int
part2 = median . go []
  where
    go score [] = score
    go score (l:ls) = case parseLine l of
      Left _ -> go score ls
      Right remainder -> go (getPoints remainder : score) ls
    getPoints = foldl (\s c -> 5 * s + p c) 0
    p '(' = 1
    p '[' = 2
    p '{' = 3
    p '<' = 4
    p _ = 0

    median xs =
      let
        xs' = sort xs
        l   = length xs `div` 2
      in xs' !! l

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ part1 input
  print $ part2 input