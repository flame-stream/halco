{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}

module Calco.CGraph where

import           Calco.Conts
import           Calco.Defs
import           Data.Function ((&))
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

data Node i o where
  Stream :: OutCont o => o -> Node i o
  Tfm1 :: (InCont i, OutCont o) => i -> o -> Node i o
  Tfm2 :: (InCont i, OutCont o) => i -> i -> o -> Node i o

-- Set of streams and transformations, annotated with contracts
newtype Env i o = Env (Map NodeName (Node i o))

-- Data sinks
type Semantics = Set NodeName

type CGraph i o = (Env i o, Semantics)

env :: [(NodeName, Node i o)] -> Env i o
env = Env . Map.fromList

stream :: Env i o -> NodeName -> o
stream (Env m) nn = m ! nn & \case
  Stream o -> o
  _        -> error $ nn <> " is expected to be stream"

tfm1 :: Env i o -> NodeName -> (i, o)
tfm1 (Env m) nn = m ! nn & \case
  Tfm1 i o -> (i, o)
  _        -> error $ nn <> " is expected to be transform 1"

tfm2 :: Env i o -> NodeName -> (i, i, o)
tfm2 (Env m) nn = m ! nn & \case
  Tfm2 i1 i2 o -> (i1, i2, o)
  _            -> error $ nn <> " is expected to be transform 2"

semantics :: [NodeName] -> Semantics
semantics = Set.fromList