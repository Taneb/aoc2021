module Main where

import Data.Maybe
import Data.Monoid
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

import Debug.Trace

data Instr
  = Forward Int
  | Down Int
  | Up Int

part1 :: [Instr] -> Int
part1 instrs = let (Sum x, Sum y) = foldMap interpret instrs in x * y
  where
    interpret (Forward x) = (Sum x, 0)
    interpret (Down y) = (0, Sum y)
    interpret (Up y) = (0, Sum (-y))

part2 :: [Instr] -> Int
part2 instrs = let (x, y, _) = appEndo (getDual (foldMap interpret instrs)) (0, 0, 0) in x * y
  where
    interpret :: Instr -> Dual (Endo (Int, Int, Int))
    interpret (Forward d) = Dual $ Endo $ \(x, y, a) -> (x + d, y + a * d, a)
    interpret (Down d)    = Dual $ Endo $ \(x, y, a) -> (x, y, a + d)
    interpret (Up d)      = Dual $ Endo $ \(x, y, a) -> (x, y, a - d)

matcher :: RE Char Instr
matcher = (string "forward " *> (Forward <$> decimal)) <|> (string "down " *> (Down <$> decimal)) <|> (string "up " *> (Up <$> decimal))

parse :: String -> [Instr]
parse xs = [instr | l <- lines xs, let Just instr = l =~ matcher]

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ part1 input
  print $ part2 input
