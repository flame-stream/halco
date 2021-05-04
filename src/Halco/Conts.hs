{-# LANGUAGE ConstraintKinds        #-}
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
  toState :: o -> s

class (Empty o, State s) => OutCont1 s o | o -> s where
  update1 :: s -> o -> s

class (Empty o, State s) => OutCont2 s o | o -> s where
  update2 :: (s, s) -> o -> s

type ContsContext s i o o1 o2 =
  ( InCont s i, OutCont s o
  , OutCont1 s o1, OutCont2 s o2 )
