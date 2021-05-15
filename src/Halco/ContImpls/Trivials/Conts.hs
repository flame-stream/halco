{-# LANGUAGE MultiParamTypeClasses #-}

module Halco.ContImpls.Trivials.Conts where

import           Halco.Utils.Classes            (Empty (..))

import qualified Halco.ContImpls.Trivial.Conts  as Trivial
import           Halco.ContImpls.Trivial.Scheme (Scheme)
import qualified Halco.Conts                    as Conts

newtype InCont = InCont [Trivial.InCont]

instance Empty InCont where
  empty = InCont [empty]

instance Conts.InCont Scheme InCont where
  match s (InCont is) = any (Conts.match s) is
