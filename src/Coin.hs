module Coin where

import System.Random

data Coin = Heads | Tails deriving Show

-- |Randomly returns Heads or Tails
flipCoin :: IO Coin
flipCoin = fmap boolToCoin (randomIO :: IO Bool) 

-- |Transforms a Bool into a Coin
boolToCoin :: Bool -> Coin
boolToCoin True = Heads
boolToCoin False = Tails
