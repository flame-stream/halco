{-# LANGUAGE TupleSections #-}

module Calco.Examples.SplashTime where

import           Data.Map          (Map)

import           Calco.CGraph      (CGraph, Env (Env), Semantics)
import qualified Calco.CGraph      as CGraph
import           Calco.Conts.Impl
import qualified Calco.Conts.Impl  as Impl
import           Calco.Conts.Types
import           Calco.DSL
import           Calco.Eval
import           Calco.Graph

semantics :: Semantics
semantics = s []  -- TODO

cgraph :: CGraph Impl.InCont Impl.OutCont
cgraph = (, semantics) $ Env
  { CGraph.streams = m
    [ "frontLogs" `ap0` emptyOut { attrsO = NewAttrs $ attrs' "front" ["id", "userId", "ts"] }
    , "backLogs"  `ap0` emptyOut { attrsO = NewAttrs $ attrs' "back" ["userId", "ts", "payload"] }
    ]
  , CGraph.tfms1 = m []  -- TODO
  , CGraph.tfms2 = m []  -- TODO
  }

graph :: Graph
graph = undefined

graph' :: Graph
graph' = undefined

type E = Map String String

-- env' :: Env' E
-- env' = Env'
--   { streams = m
--     [ "wow" ->> []
--     ]
--   , tfms1 = undefined
--   , tfms2 = undefined
--   }
