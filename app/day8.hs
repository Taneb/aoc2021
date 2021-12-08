module Main where

import SAT.MiniSat
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

import Debug.Trace

part1 :: [[String]] -> Int
part1 = sum . map f1
  where
    f1 [] = error "divider not found"
    f1 ("|":xs) = f2 xs
    f1 (_:xs) = f1 xs

    f2 [] = 0
    f2 (x:xs)
      | length x `elem` [2,3,4,7] = 1 + f2 xs
      | otherwise = f2 xs

part2 :: [[String]] -> Int
part2 = sum . map f1
  where
    f1 xs = 
      let 
        (signal, digits) = splitDigits [] xs; cond = isSegment :&&: condition signal 
      in case solve cond of
      Nothing -> error "Could not find solution"
      Just solution ->
        let assignment = understandSolution1 solution
        in traceShow assignment $ foldl (\r a -> 10 * r + a) 0 $ map (understandSolution2 assignment) digits

    splitDigits :: [String] -> [String] -> ([String], [String])
    splitDigits xs ("|":ys) = (xs, ys)
    splitDigits xs (x:ys) = splitDigits (x:xs) ys
    splitDigits _ _ = error "digits not found"

    getDigits [] = error "digits not found"
    getDigits ("|":xs) = xs
    getDigits (_:xs) = getDigits xs

    isSegment :: Formula (Char, Char)
    isSegment = All [ExactlyOne [Var (x, y) | y <- "abcdefg"] | x <- "abcdefg"]

    two :: [Char] -> Formula (Char, Char)
    two cs = Some [All (zipWith (curry Var) cs' "cf") | cs' <- permutations cs]

    three :: [Char] -> Formula (Char, Char)
    three cs = Some [All (zipWith (curry Var) cs' "acf") | cs' <- permutations cs]

    four :: [Char] -> Formula (Char, Char)
    four cs = Some [All (zipWith (curry Var) cs' "bcdf") | cs' <- permutations cs]

    five :: [Char] -> Formula (Char, Char)
    five cs = Some $ concat
      [ [All (zipWith (curry Var) cs' "acdeg") | cs' <- permutations cs]
      , [All (zipWith (curry Var) cs' "acdfg") | cs' <- permutations cs]
      , [All (zipWith (curry Var) cs' "abdfg") | cs' <- permutations cs]
      ]

    six :: [Char] -> Formula (Char, Char)
    six cs = Some $ concat
      [ [All (zipWith (curry Var) cs' "abcefg") | cs' <- permutations cs]
      , [All (zipWith (curry Var) cs' "abdefg") | cs' <- permutations cs]
      , [All (zipWith (curry Var) cs' "abcdfg") | cs' <- permutations cs]
      ]

    -- this gives us no information. Maybe I should just remove it
    seven :: [Char] -> Formula (Char, Char)
    seven cs = Some [All (zipWith (curry Var) cs' "abcdefg") | cs' <- permutations cs]

    condition1 :: String -> Formula (Char, Char)
    condition1 s = case length s of
      2 -> two s
      3 -> three s
      4 -> four s
      5 -> five s
      6 -> six s
      _ -> Yes 

    condition :: [String] -> Formula (Char, Char)
    condition = All . map condition1

    understandSolution1 :: Map (Char, Char) Bool -> Map Char Char
    understandSolution1 = M.fromList . map fst . filter snd . M.toList

    understandSolution2 :: Map Char Char -> String -> Int
    understandSolution2 m s = case sort . map (fromJust . flip M.lookup m) $ s of
      "abcefg" -> 0
      "cf" -> 1
      "acdeg" -> 2
      "acdfg" -> 3
      "bcdf" -> 4
      "abdfg" -> 5
      "abdefg" -> 6
      "acf" -> 7
      "abcdefg" -> 8
      "abcdfg" -> 9
      _ -> error "unrecognized digit"

main :: IO ()
main = do
  input <- map words . lines <$> getContents
  print $ part1 input
  print $ part2 input
