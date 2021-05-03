{-# LANGUAGE FunctionalDependencies #-}

module Halco.Conts where

import           Halco.State         (State)
import           Halco.Utils.Classes (Empty (..))

class (Empty i, State s) => InCont s i | i -> s where
  match :: s -> i -> Bool

  matchM :: s -> i -> Either (s, i) ()
  matchM s i | match s i = Right  ()
             | otherwise = Left (s, i)

class (Empty o, State s) => OutCont s o | o -> s where
  update :: s -> o -> s

  toState :: o -> s
  toState = update empty
