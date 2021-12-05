module Main where

import Data.Bifunctor (first)
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Board = [[Maybe Int]]

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
    parseBoard = map (map Just) <$> count 5 (count 5 (space *> L.decimal) <* newline)

markNumber :: Int -> Board -> Board
markNumber c = map (map (\x -> if x == Just c then Nothing else x))

checkVictory :: Board -> Bool
checkVictory b = checkRows b || checkCols b
  where
    checkRows = any (all isNothing)
    checkCols = checkRows . transpose

boardTurnAndScore :: Board -> [Int] -> (Int, Int)
boardTurnAndScore _ [] = error "Unreachable"
boardTurnAndScore b (c:cs) =
  let b' = markNumber c b
  in if checkVictory b'
    then (0, c * sum (catMaybes $ concat b'))
    else first (+1) $ boardTurnAndScore b' cs

combined :: [Int] -> [Board] -> (Int, Int)
combined calls bs0 =
  let ((_, p1),(_, p2)) = go ((100, 0),(0, 0)) bs0
  in (p1, p2)
  where
    go :: ((Int, Int), (Int, Int)) -> [Board] -> ((Int, Int), (Int, Int))
    go p [] = p
    go (p1@(d1, _), p2@(d2, _)) (b:bs) = case boardTurnAndScore b calls of
      pc@(dc, _) -> go (if dc < d1 then pc else p1, if d2 < dc then pc else p2) bs

main :: IO ()
main = do
  Just (cs, bs) <- parseMaybe parseInput <$> getContents
  let (p1, p2) = combined cs bs
  print p1
  print p2
