{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Data.Char (digitToInt)
import Data.Finite (Finite, getFinite)
import qualified Data.Vector.Sized as VS
import Data.List ( delete )
import Data.Monoid
import GHC.TypeLits
import Data.Maybe

stepP1 :: VS.Vector m (VS.Vector n Int) -> (Sum Int, VS.Vector m (VS.Vector n Int))
stepP1 = VS.mapM $ VS.mapM $ \x -> if x == 9 then (Sum 1, 10) else (Sum 0, x + 1)

adjs :: (KnownNat m, KnownNat n) => (Finite m, Finite n) -> [(Finite m, Finite n)]
adjs (x, y) = delete (x, y) [(x', y') | x' <- xs, y' <- ys]
  where
    xs = [x - 1 | getFinite x /= 0] ++ [x] ++ [x + 1 | getFinite x /= natVal x - 1]
    ys = [y - 1 | getFinite y /= 0] ++ [y] ++ [y + 1 | getFinite y /= natVal y - 1]

index2 :: VS.Vector m (VS.Vector n a) -> Finite m -> Finite n -> a
index2 grid = VS.index . VS.index grid

stepP2 :: (KnownNat m, KnownNat n) => VS.Vector m (VS.Vector n Int) -> (Sum Int, VS.Vector m (VS.Vector n Int))
stepP2 grid = VS.imapM (VS.imapM . go) grid
  where
    go i j here
      | here > 9 = (Sum 0, 11)
      | otherwise =
      let
        around = map (uncurry (index2 grid)) (adjs (i, j))
        here' = here + length (filter (== 10) around)
      in if here' > 9 then (Sum 1, 10) else (Sum 0, here')

stepP' :: (KnownNat m, KnownNat n) => VS.Vector m (VS.Vector n Int) -> Sum Int -> (Sum Int, VS.Vector m (VS.Vector n Int))
stepP' grid soFar =
  let (n, grid') = stepP2 grid
  in if n == 0
      then (soFar, grid')
      else stepP' grid' (soFar <> n)

cleanUp :: VS.Vector m (VS.Vector n Int) -> VS.Vector m (VS.Vector n Int)
cleanUp = VS.map $ VS.map cleanUp1
  where
    cleanUp1 n
      | n > 9 = 0
      | otherwise = n

stepP :: (KnownNat m, KnownNat n) => VS.Vector m (VS.Vector n Int) -> (Sum Int, VS.Vector m (VS.Vector n Int))
stepP grid =
  let (n, grid') = stepP1 grid
  in fmap cleanUp $ if n == 0
    then (Sum 0, grid')
    else stepP' grid' n

part1 :: (KnownNat m, KnownNat n) => VS.Vector m (VS.Vector n Int) -> Int
part1 grid = getSum . fst $ iterate (>>= stepP) (pure grid) !! 100

part2 :: (KnownNat (1 + m), KnownNat n) => VS.Vector (1 + m) (VS.Vector n Int) -> Int 
part2 grid = case stepP grid of
  (Sum flashes, grid')
    | flashes == VS.length grid * VS.length (VS.head grid) -> 1
    | otherwise -> 1 + part2 grid'

parse :: [String] -> Maybe (VS.Vector 10 (VS.Vector 10 Int))
parse ls = VS.fromList =<< mapM (VS.fromList . map digitToInt) ls

main :: IO ()
main = do
  Just grid <- parse . lines <$> getContents 
  print $ part1 grid
  print $ part2 grid