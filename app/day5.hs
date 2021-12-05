module Main where

import qualified Data.Map.Strict as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- several primitive tribes of computer scientists have independentally decided
-- that this is the only number system you really need. They're right.
data OneOrMany
  = One
  | Many
  deriving (Eq)

instance Semigroup OneOrMany where
  _ <> _ = Many

data Line = Line
  { start :: (Int, Int)
  , end   :: (Int, Int)
  }
  deriving (Show)

type Parser = Parsec Void String

parseLine :: Parser Line
parseLine = do
  x1 <- L.decimal
  char ','
  y1 <- L.decimal
  string " -> "
  x2 <- L.decimal
  char ','
  y2 <- L.decimal
  pure $ Line (x1, y1) (x2, y2)

parseInput :: Parser [Line]
parseInput = sepEndBy parseLine newline

orthagonalPoints :: Line -> [(Int, Int)]
orthagonalPoints (Line (x1, y1) (x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2..max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2..max x1 x2]]
  | otherwise = []


linePoints :: Line -> [(Int, Int)]
linePoints (Line (x1, y1) (x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2..max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2..max x1 x2]]
  | x1 <  x2, y1 <  y2 = zip [x1..x2] [y1..y2]
  | x1 <  x2, y1 >  y2 = zip [x1..x2] [y1, y1-1..y2]
  | x1 >  x2, y1 <  y2 = zip [x1, x1-1..x2] [y1..y2]
  | x1 >  x2, y1 >  y2 = zip [x2..x1] [y2..y1]

general :: (Line -> [(Int, Int)]) -> [Line] -> Int
general getPoints = countRepeats . inputLines
  where
    countRepeats :: M.Map (Int, Int) OneOrMany -> Int
    countRepeats = length . filter (== Many) . M.elems

    inputLines :: [Line] -> M.Map (Int, Int) OneOrMany
    inputLines = foldr (\l m -> foldr (\p -> M.insertWith (<>) p One) m (getPoints l)) M.empty

part1 :: [Line] -> Int
part1 = general orthagonalPoints

part2 :: [Line] -> Int
part2 = general linePoints

main :: IO ()
main = do
  Just ls <- parseMaybe parseInput <$> getContents
  print $ part1 ls
  print $ part2 ls
