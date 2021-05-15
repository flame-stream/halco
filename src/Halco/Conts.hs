{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Halco.Conts where

import           Halco.Scheme        (Scheme)
import           Halco.Utils.Classes (Empty (..))

class (Empty i, Scheme s) => InCont s i | i -> s where
  match :: s -> i -> Bool

  matchM :: s -> i -> Either (s, i) ()
  matchM s i | match s i = Right ()
             | otherwise = Left (s, i)

class (Empty o, Scheme s) => OutCont s o | o -> s where
  toScheme :: o -> s

class (Empty o, Scheme s) => OutCont1 s o | o -> s where
  update1 :: s -> o -> s

class (Empty o, Scheme s) => OutCont2 s o | o -> s where
  update2 :: (s, s) -> o -> s

type ContsContext s i o o1 o2 =
  ( InCont s i, OutCont s o
  , OutCont1 s o1, OutCont2 s o2 )
