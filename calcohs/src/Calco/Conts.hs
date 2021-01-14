{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Calco.Conts where

import           Data.Kind   (Type)

import           Calco.State

-- Error type of the context match.
-- Contains match input that caused error.
type ContMatchError a p i = (State a p, i)

class StateContext (ATi i) (PTi i) => InCont i where
  type ATi i :: Type -- Attribute type
  type PTi i :: Type -- Property type

  -- Input contract that allows any input
  emptyIn :: i
  match :: State (ATi i) (PTi i) -> i -> Bool

  matchM :: State (ATi i) (PTi i) -> i
         -> Either (ContMatchError (ATi i) (PTi i) i) ()
  matchM s c | match s c = Right ()
             | otherwise = Left (s, c)

type InContContext a p i = (InCont i, a ~ ATi i, p ~ PTi i)

class StateContext (ATo c) (PTo c) => OutCont c where
  type ATo c :: Type -- Attribute type
  type PTo c :: Type -- Property type

  -- Contract of the identity function
  emptyOut :: c
  update :: State (ATo c) (PTo c) -> c -> State (ATo c) (PTo c)

  toState :: c -> State (ATo c) (PTo c)
  toState = update empty

type OutContContext a p o = (OutCont o, a ~ ATo o, p ~ PTo o)

-- Proof that types of attributes and properties
-- associated with i and o are equal to a and p
type ContContext a p i o = (InContContext a p i, OutContContext a p o)
