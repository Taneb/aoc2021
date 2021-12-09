module Main where

import Control.Monad
import Control.Monad.ST
import Data.Char
import qualified Data.Ord as Ord
import Data.STRef
import Data.List

adjacents :: [[a]] -> [(a, [a])]
adjacents (as:bs:rss) =
  l1 as bs ++ l2 as bs rss
  where
    l1 (a1:a2:as) (b:bs) = (a1, [a2, b]) : l11 a1 a2 as bs
    l1 _ _ = error "Nonsquare?"
    l11 a1 a2 [] [b] = [(a2, [a1, b])]
    l11 a1 a2 (a3:as) (b:bs) = (a2, [a1,b,a3]) : l11 a2 a3 as bs
    l11 _ _ _ _ = error "Nonsquare??"
    l2 (a:as) (b1:b2:bs) ((c:cs):r) = (b1, [a, c, b2]) : l21 as b1 b2 bs cs ++ l2 (b1:b2:bs) (c:cs) r
    l2 (a:as) (b1:b2:bs) [] = (b1, [a, b1]) : l22 as b1 b2 bs
    l2 _ _ _ = error "Nonsquare???"
    l21 [a] b1 b2 [] [c] = [(b2, [a, b1, c])]
    l21 (a:as) b1 b2 (b3:bs) (c:cs) = (b2, [a, b1, c, b3]) : l21 as b2 b3 bs cs
    l21 _ _ _ _ _ = error "Nonsquare????"
    l22 (a:as) b1 b2 (b3:bs) = (b2, [a,b1,b3]) : l22 as b2 b3 bs
    l22 [a] b1 b2 [] = [(b2, [a, b1])]
    l22 _  _ _ _ = error "Nonsquare?????"
adjacents _ = error "Really expected more input"

part1 :: [[Int]] -> Int
part1 = sum . map ((+1) . fst) . filter (\(x, adjs) -> x < minimum adjs) . adjacents

data SUF' s
  = Root Int -- size of set
  | Down (STRef s (SUF' s))

type SUF s = STRef s (SUF' s)

newSUF :: ST s (SUF s)
newSUF = newSTRef (Root 1)

findRoot :: SUF s -> ST s (SUF s, Int)
findRoot suf = do
  r <- readSTRef suf
  case r of
    Root s -> pure (suf, s)
    Down n -> do
      (root, s) <- findRoot n
      writeSTRef suf (Down root)
      pure (root, s)

unionSufs :: SUF s -> SUF s -> ST s ()
unionSufs suf1 suf2 = do
  (root1, size1) <- findRoot suf1
  (root2, size2) <- findRoot suf2
  when (root1 /= root2) $ if size1 <= size2
    then do
      writeSTRef root1 $ Down root2
      writeSTRef root2 $ Root (size1 + size2)
    else do
      writeSTRef root2 $ Down root1
      writeSTRef root1 $ Root (size1 + size2)

part2 :: [[Int]] -> Int
part2 xs = runST $ count <=< combine <=< initialize $ xs
  where
    count :: [SUF s] -> ST s Int
    count = fmap (product . take 3 . sortOn Ord.Down . map snd . nub) . mapM findRoot

    combine :: [(Maybe (SUF s), [Maybe (SUF s)])] -> ST s [SUF s]
    combine [] = pure []
    combine ((Nothing, _):r) = combine r
    combine ((Just suf, adjs):r) = (:) <$> combine1 suf adjs <*> combine r

    combine1 :: SUF s -> [Maybe (SUF s)] -> ST s (SUF s)
    combine1 suf1 [] = pure suf1
    combine1 suf1 (Nothing:r) = combine1 suf1 r
    combine1 suf1 (Just suf2:r) = unionSufs suf1 suf2 *> combine1 suf1 r

    initialize :: [[Int]] -> ST s [(Maybe (SUF s), [Maybe (SUF s)])]
    initialize = fmap adjacents . mapM (mapM initialize1)

    initialize1 :: Int -> ST s (Maybe (SUF s))
    initialize1 9 = pure Nothing
    initialize1 _ = Just <$> newSUF

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> getContents 
  print $ part1 input
  print $ part2 input
