{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Calco.Conts where

import           Data.Kind   (Type)

import           Calco.State

-- Error type of the context match.
-- Contains match input that caused error.
type ContMatchError a p i = (State a p, i)

class StateContext (AT i) (PT i) => InCont i where
  type AT i :: Type -- Attribute type
  type PT i :: Type -- Property type

  -- Input contract that allows any input
  emptyIn :: i
  match :: State (AT i) (PT i) -> i -> Bool

  matchM :: State (AT i) (PT i) -> i
         -> Either (ContMatchError (AT i) (PT i) i) ()
  matchM s c | match s c = Right ()
             | otherwise = Left (s, c)

type InContContext a p i = (InCont i, a ~ AT i, p ~ PT i)

class StateContext (AT' c) (PT' c) => OutCont c where
  type AT' c :: Type -- Attribute type
  type PT' c :: Type -- Property type

  -- Contract of the identity function
  emptyOut :: c
  update :: State (AT' c) (PT' c) -> c -> State (AT' c) (PT' c)

  toState :: c -> State (AT' c) (PT' c)
  toState = update empty

type OutContContext a p o = (OutCont o, a ~ AT' o, p ~ PT' o)

-- Proof that types of attributes and properties
-- associated with i and o are equal to a and p
type ContContext a p i o = (InContContext a p i, OutContContext a p o)
