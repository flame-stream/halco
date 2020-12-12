{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Calco.Conts where

import           Calco.State

type ContMatchError a p i = (State a p, i)

class StateContext (AT i) (PT i) => InCont i where
  type AT i :: * -- Attribute type
  type PT i :: * -- Property type

  emptyIn :: i
  match :: State (AT i) (PT i) -> i -> Bool

  matchM :: State (AT i) (PT i) -> i
         -> Either (ContMatchError (AT i) (PT i) i) ()
  matchM s c | match s c = Right ()
             | otherwise = Left (s, c)

type InContContext a p i = (a ~ AT i, p ~ PT i)

class StateContext (AT' c) (PT' c) => OutCont c where
  type AT' c :: * -- Attribute type
  type PT' c :: * -- Property type

  emptyOut :: c
  update :: State (AT' c) (PT' c) -> c -> State (AT' c) (PT' c)

type OutContContext a p o = (a ~ AT' o, p ~ PT' o)

type ContContext a p i o = (InContContext a p i, OutContContext a p o)
