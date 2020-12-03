module Calco.Check (checkGraph) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Function ((&))
import Control.Monad (foldM)

import Calco.Graph
import Calco.StreamState


type CheckedTerms = Map TermMarker State

checkGraph :: Env -> Graph -> Either Error ()
checkGraph e g@(Graph m) = either Left (const $ Right ())
                         . foldM (checkTerm e g) Map.empty
                         $ Map.keys m

checkTerm :: Env -> Graph
          -> CheckedTerms -> TermMarker
          -> Either Error CheckedTerms
checkTerm e g checked tm = snd <$> checkTermHelper e g checked tm

checkTermHelper :: Env -> Graph
                -> CheckedTerms -> TermMarker
                -> Either Error (State, CheckedTerms)
checkTermHelper e g@(Graph m) checked tm
  | tm `Map.member` checked = Right (checked ! tm, checked)
  | otherwise = case m ! tm of
    Const stream ->
      let state = updateState emptyState $ (e & streams) ! stream in
      let checked' = check state checked in
      Right (state, checked')
    App1 f tm' -> do
      let (inCont, outCont) = (e & tfms1) ! f
      (state, checked') <- checkTermHelper e g checked tm'
      matchStateM state inCont
      let state' = updateState state outCont
      let checked'' = check state' checked'
      Right (state', checked'')
    App2 f tm1 tm2 -> do
      let (inCont1, inCont2, outCont) = (e & tfms2) ! f
      (state1, checked') <- checkTermHelper e g checked tm1
      matchStateM state1 inCont1
      (state2, checked'') <- checkTermHelper e g checked' tm2
      matchStateM state2 inCont2
      let state = state1 <> state2
      let state' = updateState state outCont
      let checked''' = check state' checked''
      Right (state', checked''')
  where
    check :: State -> CheckedTerms -> CheckedTerms
    check = Map.insert tm
