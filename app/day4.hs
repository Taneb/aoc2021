module Main where

import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Board = [[(Bool, Int)]]

type Parser = Parsec Void String

parseInput = do
  cs <- parseCalls
  _ <- newline
  _ <- newline
  bs <- parseBoards
  pure (cs, bs)
  where
    parseCalls :: Parser [Int]
    parseCalls = sepBy L.decimal (char ',')
    parseBoards :: Parser [Board]
    parseBoards = sepBy parseBoard newline
    parseBoard :: Parser Board
    parseBoard = map (map ((,) False)) <$> count 5 (count 5 (space *> L.decimal) <* newline)

markNumber :: Int -> Board -> Board
markNumber c = map (map (\x -> if snd x == c then (True, c) else x))

checkVictory :: Board -> Bool
checkVictory b = checkRows b || checkCols b
  where
    checkRows = any (all fst)
    checkCols = checkRows . transpose

part1 :: [Board] -> [Int] -> Int
part1 bs (c:cs) =
  let bs' = map (markNumber c) bs
  in case find checkVictory bs' of
    Nothing -> part1 bs' cs
    Just v  -> c * sum (map snd . filter (not . fst) $ concat v)

part2 :: [Board] -> [Int] -> Int
part2 bs (c:cs) =
  let bs' = map (markNumber c) bs
  in case filter (not . checkVictory) bs' of
    [b] -> part1 [b] cs
    _   -> part2 bs' cs

main :: IO ()
main = do
  Just (cs, bs) <- parseMaybe parseInput <$> getContents
  print $ part1 bs cs
  print $ part2 bs cs
