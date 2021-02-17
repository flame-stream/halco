{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}

module Calco.CGraph where

import           Data.Function     ((&))
import           Data.Map          (Map, (!))
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           Calco.Conts.Types (InCont, OutCont)
import           Calco.Defs        (NodeName)

data Node i o where
  Stream :: OutCont o => o -> Node i o
  Tfm1 :: (InCont i, OutCont o) => i -> o -> Node i o
  Tfm2 :: (InCont i, OutCont o) => i -> i -> o -> Node i o

-- Set of streams and transformations, annotated with contracts
newtype Env i o = Env (Map NodeName (Node i o))

type Semantics = Set NodeName

type CGraph i o = (Env i o, Semantics)

env :: [(NodeName, Node i o)] -> Env i o
env = Env . Map.fromList

semantics :: [NodeName] -> Semantics
semantics = Set.fromList

envToList :: Env i o -> [(NodeName, Node i o)]
envToList (Env m) = Map.toList m

nodeNamesP :: (Node i o -> Bool) -> Env i o -> [NodeName]
nodeNamesP p = map fst . filter (p . snd) . envToList

streams :: Env i o -> [NodeName]
streams = nodeNamesP isStream

tfms :: Env i o -> [NodeName]
tfms = nodeNamesP isTfm

tfms1 :: Env i o -> [NodeName]
tfms1 = nodeNamesP isTfm1

tfms1C :: Env i o -> [(i, o)]
tfms1C e = map (tfm1 e) (tfms1 e)

tfms2 :: Env i o -> [NodeName]
tfms2 = nodeNamesP isTfm2

tfms2C :: Env i o -> [(i, i, o)]
tfms2C e = map (tfm2 e) (tfms2 e)

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

isStream :: Node i o -> Bool
isStream = \case
  Stream _ -> True
  _        -> False

isTfm :: Node i o -> Bool
isTfm n = isTfm1 n || isTfm2 n

isTfm1 :: Node i o -> Bool
isTfm1 = \case
  Tfm1 {} -> True
  _       -> False

isTfm2 :: Node i o -> Bool
isTfm2 = \case
  Tfm2 {} -> True
  _       -> False
