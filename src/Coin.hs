{-|
Module      : Coin
Description : Provides the ability to flip coins.
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module Coin where

import System.Random

-- |Represents a flipped coin. Can be either heads or tails.
data Coin = Heads | Tails deriving Show

-- |Randomly returns Heads or Tails
flipCoin :: IO Coin
flipCoin = fmap boolToCoin (randomIO :: IO Bool) 

-- |Transforms a Bool into a Coin
boolToCoin :: Bool -> Coin
boolToCoin True = Heads
boolToCoin False = Tails
