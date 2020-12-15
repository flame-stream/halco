{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Calco.Check (checkGraph) where

import           Calco.CoGraph
import           Calco.Conts
import           Calco.Graph
import           Calco.State             (State)
import qualified Calco.State             as State
import           Control.Monad           (foldM)
import           Data.Either.Combinators (mapLeft)
import           Data.Map                (Map, (!))
import qualified Data.Map                as Map
import           Data.Set                (isSubsetOf)

type CheckedTerms a p = Map TermMarker (State a p)

data CheckGraphError a p i =
    CME (ContMatchError a p i)
  | SemanticError
  deriving (Show)

checkGraph :: ContContext a p i o
           => CoGraph i o -> Graph -> Either (CheckGraphError a p i) ()
checkGraph (e, s) g@(Graph m)
  | s `isSubsetOf` nodeNames g =
    mapLeft CME $ () <$ foldM (checkTerm e g) Map.empty (Map.keys m)
  | otherwise = Left SemanticError

checkTerm :: ContContext a p i o
          => Env i o -> Graph
          -> CheckedTerms a p -> TermMarker
          -> Either (ContMatchError a p i) (CheckedTerms a p)
checkTerm e g checked tm = snd <$> checkTermHelper e g checked tm

checkTermHelper :: ContContext a p i o
                => Env i o -> Graph
                -> CheckedTerms a p -> TermMarker
                -> Either (ContMatchError a p i) (State a p, CheckedTerms a p)
checkTermHelper e g@(Graph m) checked tm
  | tm `Map.member` checked = Right (checked ! tm, checked)
  | otherwise = case m ! tm of
    Const s -> do
      let state =  State.empty `update` stream e s
      let checked' = check state checked
      Right (state, checked')
    App1 f tm' -> do
      let (inCont, outCont) = tfm1 e f
      (state, checked') <- checkTermHelper e g checked tm'
      state `matchM` inCont
      let state' = state `update` outCont
      let checked'' = check state' checked'
      Right (state', checked'')
    App2 f tm1 tm2 -> do
      let (inCont1, inCont2, outCont) = tfm2 e f
      (state1, checked') <- checkTermHelper e g checked tm1
      state1 `matchM` inCont1
      (state2, checked'') <- checkTermHelper e g checked' tm2
      state2 `matchM` inCont2
      let state = state1 <> state2
      let state' = state `update` outCont
      let checked''' = check state' checked''
      Right (state', checked''')
  where
    check :: State a p -> CheckedTerms a p -> CheckedTerms a p
    check = Map.insert tm
