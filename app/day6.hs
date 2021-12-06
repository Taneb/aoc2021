{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Linear
import Linear.V

-- We can do this with matrices!

-- Here's a matrix that represents the population change in a single day
-- When you multiply by a column vector containing the number of lanternfish
-- with each possible timer value, it gives you another vector containing the
-- amounts for the subsequent days.
gen1Mat :: V 9 (V 9 Integer)
gen1Mat =
  V [ V [0,1,0,0,0,0,0,0,0]
    , V [0,0,1,0,0,0,0,0,0]
    , V [0,0,0,1,0,0,0,0,0]
    , V [0,0,0,0,1,0,0,0,0]
    , V [0,0,0,0,0,1,0,0,0]
    , V [0,0,0,0,0,0,1,0,0]
    , V [1,0,0,0,0,0,0,1,0]
    , V [0,0,0,0,0,0,0,0,1]
    , V [1,0,0,0,0,0,0,0,0]
    ]

-- We can work out corresponding matrices that work out, rather than the
-- population change after one day, the change after any number of days, by
-- raising 'gen1Mat' to the power of the number of days desired. I precompute
-- the matrices needed for the problem. Here's gen1Mat ^ 80
gen80Mat :: V 9 (V 9 Int)
gen80Mat =
  V [ V [252, 20,210, 37,120, 84, 45,126, 11]
    , V [ 56,252, 20,210, 37,120, 84, 45,126]
    , V [210, 56,252, 20,210, 37,120, 84, 45]
    , V [165,210, 56,252, 20,210, 37,120, 84]
    , V [121,165,210, 56,252, 20,210, 37,120]
    , V [330,121,165,210, 56,252, 20,210, 37]
    , V [ 57,330,121,165,210, 56,252, 20,210]
    , V [210, 37,120, 84, 45,126, 11,126,  9]
    , V [ 20,210, 37,120, 84, 45,126, 11,126]
    ]

-- And here's gen1Mat^256. Pretty big numbers, aren't they.
gen256Mat :: V 9 (V 9 Int)
gen256Mat =
  V [ V [ 655568076, 496266131,589731885,399865906,491122368,357868865,378763547,339582910,280698774]
    , V [ 659462321, 655568076,496266131,589731885,399865906,491122368,357868865,378763547,339582910]
    , V [ 697451775, 659462321,655568076,496266131,589731885,399865906,491122368,357868865,378763547]
    , V [ 869885915, 697451775,659462321,655568076,496266131,589731885,399865906,491122368,357868865]
    , V [ 757734771, 869885915,697451775,659462321,655568076,496266131,589731885,399865906,491122368]
    , V [1080854253, 757734771,869885915,697451775,659462321,655568076,496266131,589731885,399865906]
    , V [ 896132037,1080854253,757734771,869885915,697451775,659462321,655568076,496266131,589731885]
    , V [ 589731885, 399865906,491122368,357868865,378763547,339582910,280698774,315985166,215567357]
    , V [ 496266131, 589731885,399865906,491122368,357868865,378763547,339582910,280698774,315985166]
    ]

-- Because we're going to just be taking the sum of the product with the vector
-- to get the total number of individuals, we can rearrange a bit so we take
-- the dot product with a sort of collapse of the matrix. The linear library
-- has this function as 'sumV' but we can get away with precomputing it.
gen80Vec :: V 9 Int
gen80Vec = V [1421,1401,1191,1154,1034,950,905,779,768]

gen256Vec :: V 9 Int
gen256Vec = V [6703087164,6206821033,5617089148,5217223242,4726100874,4368232009,3989468462,3649885552,3369186778]

main :: IO ()
main = do
  input <- filter (/= ',') <$> getContents
  let
    initialPopulation :: V 9 Int
    initialPopulation 
      = V [ length $ filter (== '0') input
          , length $ filter (== '1') input
          , length $ filter (== '2') input
          , length $ filter (== '3') input
          , length $ filter (== '4') input
          , length $ filter (== '5') input
          , length $ filter (== '6') input
          , length $ filter (== '7') input
          , length $ filter (== '8') input
          ]
  print $ gen80Vec `dot` initialPopulation
  print $ gen256Vec `dot` initialPopulation
