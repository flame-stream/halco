module Calco.Check (checkGraph) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (isJust)
import Control.Monad (foldM)

import Calco.Graph
import Calco.Conts
import Calco.StreamState


checkGraph :: Env -> Graph -> Either (State, InCont) ()
checkGraph e g@(Graph m) = either Left (const $ Right ())
                         . foldM (checkTerm e g) Set.empty
                         $ Map.keys m

  -- let (i, t) = Map.findMin m of
  -- case t of
  -- Const c -> updateState emptyState <$> Map.lookup c (e & streams)
  -- App1 f t' -> do
  --   s <- checkTerm e t'
  --   (inCont, outCont) <- Map.lookup f (e & tfms1)
  --   if matchState s inCont
  --     then Just $ updateState s outCont
  --     else Nothing
  -- App2 f t1 t2 -> do
  --   s1 <- checkTerm e t1
  --   s2 <- checkTerm e t2
  --   (inCont1, inCont2, outCont) <- Map.lookup f (e & tfms2)
  --   if matchState s1 inCont1 && matchState s2 inCont2
  --     then Just $ updateState (unionState s1 s2) outCont
  --     else Nothing

checkTerm :: Env -> Graph
          -> Set TermMarker -> TermMarker
          -> Either (State, InCont) (Set TermMarker)
checkTerm e g checked tm = snd <$> checkTermHelper e g checked tm

checkTermHelper :: Env -> Graph
                -> Set TermMarker -> TermMarker
                -> Either (State, InCont) (State, Set TermMarker)
checkTermHelper e g@(Graph m) checked tm
  | tm `Set.member` checked = Right (mempty, checked)
  | otherwise = case m ! tm of
    Const sn -> Right ( updateState mempty $ (e & streams) ! sn
                      , tm `Set.insert` checked )
    App1 f tm' -> do
      (s, checked') <- undefined
      undefined
    App2 f tm1 tm2 -> undefined
