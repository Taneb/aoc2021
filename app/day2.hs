{-# Language OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Monoid
import Data.Void

data Instr
  = Forward Int
  | Down Int
  | Up Int

parse1 :: Parser Instr
parse1 = choice
  [ Forward <$> ("forward " *> decimal)
  , Down    <$> ("down "    *> decimal)
  , Up      <$> ("up "      *> decimal)
  ]
{-# INLINE parse1 #-}

getInput :: ConduitT i Instr IO ()
getInput =
  C.stdin .|
  C.conduitParser (parse1 <* endOfLine) .|
  C.map snd

data Part1State = P1State
  { p1Distance :: {-# UNPACK #-} !Int
  , p1Depth    :: {-# UNPACK #-} !Int
  }

instance Semigroup Part1State where
  P1State dist1 depth1 <> P1State dist2 depth2 = P1State (dist1 + dist2) (depth1 + depth2)
  {-# INLINE (<>) #-}

instance Monoid Part1State where
  mempty = P1State 0 0

part1 :: Monad m => ConduitT Instr o m Int
part1 = fmap (\r -> p1Distance r * p1Depth r) $ C.foldMap interpret
  where
    interpret :: Instr -> Part1State
    interpret (Forward d) = P1State d 0
    interpret (Down    d) = P1State 0 d
    interpret (Up      d) = P1State 0 (- d)
    {-# INLINE interpret #-}
{-# INLINE part1 #-}

data Part2State = P2State
  { p2Distance :: {-# UNPACK #-} !Int
  , p2Depth    :: {-# UNPACK #-} !Int
  , p2Aim      :: {-# UNPACK #-} !Int
  }

instance Semigroup Part2State where
  P2State dist1 depth1 aim1 <> P2State dist2 depth2 aim2 = P2State (dist1 + dist2) (depth1 + depth2 + dist2 * aim1) (aim1 + aim2)
  {-# INLINE (<>) #-}

instance Monoid Part2State where
  mempty = P2State 0 0 0

part2 :: Monad m => ConduitT Instr o m Int
part2 = fmap (\r -> p2Distance r * p2Depth r) $ C.foldMap interpret
  where
    interpret :: Instr -> Part2State
    interpret (Forward d) = P2State d 0 0
    interpret (Down    d) = P2State 0 0 d
    interpret (Up      d) = P2State 0 0 (- d)
    {-# INLINE interpret #-}
{-# INLINE part2 #-}

solution :: Monad m => ConduitT Instr Void m (Int, Int)
solution = C.getZipSink $ liftA2 (,) (C.ZipSink part1) (C.ZipSink part2)

main :: IO ()
main = do
  (p1, p2) <- C.runConduit (getInput .| solution)
  print p1
  print p2
