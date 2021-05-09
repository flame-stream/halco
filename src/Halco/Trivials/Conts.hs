{-# LANGUAGE MultiParamTypeClasses #-}

module Halco.Trivials.Conts where

import           Halco.Utils.Classes (Empty (..))

import qualified Halco.Conts         as Conts
import qualified Halco.Trivial.Conts as Trivial
import           Halco.Trivial.State (State)

newtype InCont = InCont [Trivial.InCont]

instance Empty InCont where
  empty = InCont []

instance Conts.InCont State InCont where
  match s (InCont is) = any (Conts.match s) is
