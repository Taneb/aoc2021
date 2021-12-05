module Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Maybe
import Data.Void

type Board = [[Maybe Int]]

parseCalls' :: Parser [Int]
parseCalls' = sepBy decimal (char ',') <* endOfLine

parseBoard' :: Parser Board
parseBoard' = do
  endOfLine
  map (map Just) <$> count 5 (count 5 (many space *> decimal) <* endOfLine)

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

solution :: [Int] -> ConduitT Board Void IO (Int, Int)
solution calls = finalize <$> C.foldl checkBoard ((100, 0), (0, 0))
  where
    finalize :: ((Int, Int), (Int, Int)) -> (Int, Int)
    finalize ((_, p1), (_, p2)) = (p1, p2)

    checkBoard (p1, p2) b =
      let pc = boardTurnAndScore b calls
      in (min p1 pc, max p2 pc)

fullSolution :: ConduitT () Void IO (Int, Int)
fullSolution = C.stdin .| do
  calls <- C.sinkParser parseCalls'
  C.conduitParser parseBoard' .| C.map snd .| solution calls

main :: IO ()
main = do
  (p1, p2) <- C.runConduit fullSolution
  print p1
  print p2
