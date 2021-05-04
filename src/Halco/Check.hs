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

import           Halco.CGraph                 (CGraph, COp1 (COp1), COp2 (COp2),
                                               CSource (CSource), Env,
                                               Semantics)
import qualified Halco.CGraph                 as CGraph
import           Halco.Conts                  (ContsContext, InCont (..),
                                               OutCont (..), OutCont1 (..),
                                               OutCont2 (..))
import           Halco.Graph                  (Graph (..), Node (..), NodeId)
import qualified Halco.Graph                  as Graph
import           Halco.State                  (State)
import           Halco.Utils.Data.Traversable (countOccs)

data CheckGraphError s i =
    ContMatchError (s, i)
  | SemanticsError
  deriving (Show)

checkGraph :: ContsContext s i o o1 o2
           => CGraph s i o o1 o2 -> Graph -> Either (CheckGraphError s i) ()
checkGraph (e, s) g@(Graph m)
  | not $ g `hasSemantics` s = Left SemanticsError
  | otherwise = mapLeft ContMatchError $ () <$ foldM (checkTerm e g) Map.empty (Map.keys m)

hasSemantics :: Graph -> Semantics -> Bool
hasSemantics g s =
  let occs = countOccs $ Graph.nodeNames g in
  all ((== (1 :: Integer)) . (occs !)) s

type CheckedTerms s = Map NodeId s

checkTerm :: ContsContext s i o o1 o2
          => Env s i o o1 o2 -> Graph
          -> CheckedTerms s -> NodeId
          -> Either (s, i) (CheckedTerms s)
checkTerm e g checked nid = snd <$> checkTermHelper e g checked nid

checkTermHelper :: ContsContext s i o o1 o2
                => Env s i o o1 o2 -> Graph
                -> CheckedTerms s -> NodeId
                -> Either (s, i) (s, CheckedTerms s)
checkTermHelper e g@(Graph m) checked nid
  | nid `Map.member` checked = Right (checked ! nid, checked)
  | otherwise = case m ! nid of
    Source nn ->
      let state = toState $ CGraph.sources e ! nn & \case CSource o -> o in
      let checked' = check state checked in
      Right (state, checked')
    Op1 nn nid' -> do
      let COp1 i o = CGraph.ops1 e ! nn
      (state, checked') <- checkTermHelper e g checked nid'
      state `matchM` i
      let state' = state `update1` o
      let checked'' = check state' checked'
      Right (state', checked'')
    Op2 nn nid1 nid2 -> do
      let COp2 i1 i2 o = CGraph.ops2 e ! nn
      (state1, checked') <- checkTermHelper e g checked nid1
      state1 `matchM` i1
      (state2, checked'') <- checkTermHelper e g checked' nid2
      state2 `matchM` i2
      let state' = (state1, state2) `update2` o
      let checked''' = check state' checked''
      Right (state', checked''')
  where
    check :: s -> CheckedTerms s -> CheckedTerms s
    check = Map.insert nid
