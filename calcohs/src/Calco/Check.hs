{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Calco.Check (checkGraph) where

import           Control.Monad           (foldM)
import           Data.Either.Combinators (mapLeft)
import           Data.Map                (Map, (!))
import qualified Data.Map                as Map
import           Data.Set                (isSubsetOf)

import           Calco.CGraph
import           Calco.Conts
import           Calco.Graph
import           Calco.State             (State)
import           Calco.Utils             (countOccs)

data CheckGraphError a p i =
    CME (ContMatchError a p i)
  | SemanticsError
  deriving (Show)

checkGraph :: ContContext a p i o
           => CGraph i o -> Graph -> Either (CheckGraphError a p i) ()
checkGraph (e, s) g@(Graph m)
  | not $ g `hasSemantics` s = Left SemanticsError
  | otherwise = mapLeft CME $ () <$ foldM (checkTerm e g) Map.empty (Map.keys m)

hasSemantics :: Graph -> Semantics -> Bool
hasSemantics g s =
  let occs = countOccs $ nodeNames g
   in all ((== (1 :: Integer)) . (occs !)) s

type CheckedTerms a p = Map TermId (State a p)

checkTerm :: ContContext a p i o
          => Env i o -> Graph
          -> CheckedTerms a p -> TermId
          -> Either (ContMatchError a p i) (CheckedTerms a p)
checkTerm e g checked tid = snd <$> checkTermHelper e g checked tid

checkTermHelper :: ContContext a p i o
                => Env i o -> Graph
                -> CheckedTerms a p -> TermId
                -> Either (ContMatchError a p i) (State a p, CheckedTerms a p)
checkTermHelper e g@(Graph m) checked tid
  | tid `Map.member` checked = Right (checked ! tid, checked)
  | otherwise = case m ! tid of
    Const s -> do
      let state = toState $ stream e s
      let checked' = check state checked
      Right (state, checked')
    App1 f tid' -> do
      let (inCont, outCont) = tfm1 e f
      (state, checked') <- checkTermHelper e g checked tid'
      state `matchM` inCont
      let state' = state `update` outCont
      let checked'' = check state' checked'
      Right (state', checked'')
    App2 f tid1 tid2 -> do
      let (inCont1, inCont2, outCont) = tfm2 e f
      (state1, checked') <- checkTermHelper e g checked tid1
      state1 `matchM` inCont1
      (state2, checked'') <- checkTermHelper e g checked' tid2
      state2 `matchM` inCont2
      let state = state1 <> state2
      let state' = state `update` outCont
      let checked''' = check state' checked''
      Right (state', checked''')
  where
    check :: State a p -> CheckedTerms a p -> CheckedTerms a p
    check = Map.insert tid
