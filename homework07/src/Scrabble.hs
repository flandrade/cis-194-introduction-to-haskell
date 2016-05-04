----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score letter
  | letter `elem` "aeilnorstu" = Score 1
  | letter `elem` "dg"         = Score 2
  | letter `elem` "bcmp"       = Score 3
  | letter `elem` "fhvwy"      = Score 4
  | letter `elem` "k"          = Score 5
  | letter `elem` "jx"         = Score 8
  | letter `elem` "qz"         = Score 10
  | otherwise                  = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
