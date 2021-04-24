{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}

module Halco.Check where

import           Control.Monad                (foldM)
import           Data.Either.Combinators      (mapLeft)
import           Data.Function                ((&))
import           Data.Map                     (Map, (!))
import qualified Data.Map                     as Map
import           Data.Set                     (isSubsetOf)

import           Halco.CGraph                 (CGraph, CSource (CSource),
                                               CTfm1 (CTfm1), CTfm2 (CTfm2),
                                               Env, Semantics)
import qualified Halco.CGraph                 as CGraph
import           Halco.Conts.Types            (ContContext, ContMatchError,
                                               InCont (..), OutCont (..))
import           Halco.Graph                  (Graph (..), Node (..), NodeId)
import qualified Halco.Graph                  as Graph
import           Halco.State                  (State)
import qualified Halco.State                  as State
import           Halco.Utils.Data.Traversable (countOccs)

data CheckGraphError a p i =
    ContMatchError (ContMatchError a p i)
  | SemanticsError
  deriving (Show)

checkGraph :: ContContext a p i o
           => CGraph i o -> Graph -> Either (CheckGraphError a p i) ()
checkGraph (e, s) g@(Graph m)
  | not $ g `hasSemantics` s = Left SemanticsError
  | otherwise = mapLeft ContMatchError $ () <$ foldM (checkTerm e g) Map.empty (Map.keys m)

hasSemantics :: Graph -> Semantics -> Bool
hasSemantics g s =
  let occs = countOccs $ Graph.nodeNames g
   in all ((== (1 :: Integer)) . (occs !)) s

type CheckedTerms a p = Map NodeId (State a p)

checkTerm :: ContContext a p i o
          => Env i o -> Graph
          -> CheckedTerms a p -> NodeId
          -> Either (ContMatchError a p i) (CheckedTerms a p)
checkTerm e g checked nid = snd <$> checkTermHelper e g checked nid

checkTermHelper :: ContContext a p i o
                => Env i o -> Graph
                -> CheckedTerms a p -> NodeId
                -> Either (ContMatchError a p i) (State a p, CheckedTerms a p)
checkTermHelper e g@(Graph m) checked nid
  | nid `Map.member` checked = Right (checked ! nid, checked)
  | otherwise = case m ! nid of
    Source nn ->
      let state = toState $ CGraph.sources e ! nn & \case CSource o -> o in
      let checked' = check state checked in
      Right (state, checked')
    Tfm1 nn nid' -> do
      let CTfm1 i o = CGraph.tfms1 e ! nn
      (state, checked') <- checkTermHelper e g checked nid'
      state `matchM` i
      let state' = state `update` o
      let checked'' = check state' checked'
      Right (state', checked'')
    Tfm2 nn nid1 nid2 -> do
      let CTfm2 i1 i2 o = CGraph.tfms2 e ! nn
      (state1, checked') <- checkTermHelper e g checked nid1
      state1 `matchM` i1
      (state2, checked'') <- checkTermHelper e g checked' nid2
      state2 `matchM` i2
      let state = state1 <> state2
      let state' = state `update` o
      let checked''' = check state' checked''
      Right (state', checked''')
  where
    check :: State a p -> CheckedTerms a p -> CheckedTerms a p
    check = Map.insert nid
