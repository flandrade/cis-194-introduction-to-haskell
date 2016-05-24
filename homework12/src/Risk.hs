{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Applicative
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Num, Ord, Show)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom


----------------------------------------------------------------------
-- Risk
----------------------------------------------------------------------

type Army =
  Int


data Battlefield =
  Battlefield
    { attackers :: Army
    , defenders :: Army
    }
  deriving (Eq, Ord, Show)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- Replicate function from lecture
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

-- The dice function simulares the roll of @n@ dice.
dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

-- Result of battle
battle :: Battlefield -> Rand StdGen Battlefield
battle x = do
  attackerDice <- dice (testMax attacker)
  defenderDice <- dice (testMax defender)
  let casualties = defineCasualties attackerDice defenderDice
  return $ resultBattle x casualties
  where attacker = attackers x
        defender = defenders x
        testMax s = if (s >= 2) then 2 else s

-- resultBattle = soldiers - casualties
resultBattle :: Battlefield -> Battlefield -> Battlefield
resultBattle soldiers casualties =
  Battlefield (attackers soldiers - attackers casualties)
              (defenders soldiers - defenders casualties)

-- Define casualties according to attacker and defender dices
defineCasualties:: [DieValue] -> [DieValue] -> Battlefield
defineCasualties attacker defender = Battlefield numAttackers numDefenders
  where res = defineResult (sort attacker) (sort defender)
        numDefenders = sum $ map fromEnum res
        numAttackers= length res - numDefenders

-- Return True if attacker wins
defineResult :: [DieValue] -> [DieValue] -> [Bool]
defineResult attacker defender =
  getZipList $ (>) <$> ZipList  attacker <*> ZipList defender


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

invade :: Battlefield -> Rand StdGen Battlefield
invade x
  | attackers x < 2 || defenders x == 0 = return x
  | otherwise                           = (battle x) >>= invade


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

successProb :: Battlefield -> Rand StdGen Double
successProb x = do
  results <- replicateM 1000 (invade x)
  let n = length . filter (\b -> (defenders b) == 0) $ results
  return $ (fromIntegral n) / (fromIntegral 1000)
