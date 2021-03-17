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
semantics = s ["splashStats"]

cgraph :: CGraph Impl.InCont Impl.OutCont
cgraph = (, semantics) $ Env
  { CGraph.streams = m
    [ "frontLogs" `ap0` emptyOut
      { attrsO = NewAttrs $ attrs' "front" ["id", "queryId", "userId", "ts"] }
    , "backLogs" `ap0` emptyOut
      { attrsO = NewAttrs $ attrs' "back" ["queryId", "userId", "ts", "payload"] }
    ]
  , CGraph.tfms1 = m
    [ "addFrontFeatures"
      `ap1` emptyIn  { attrsI = attr "front.id" }
       -->  emptyOut { attrsO = AddAttrs $ attr "frontFeatures" }

    , "addUserFeatures"
      `ap1` emptyIn  { attrsI = attr "front.userId" }
       -->  emptyOut { attrsO = AddAttrs $ attr "userFeatures" }

    , "setSessionTrigger"
      `ap1` emptyIn  { attrsI  = attrs ["frontFeatures", "userFeatures", "front.ts", "back.ts"]
                     , propsI' = prop "frontsFiltered" }
       -->  emptyOut { propsO  = prop "sessional" }

    , "filterFronts"
      `ap1` emptyIn  { attrsI = attr "front.id" }
       -->  emptyOut { propsO = prop "frontsFiltered" }

    , "splashStats"
      `ap1` emptyIn  { attrsI = attrs ["front.userId", "front.ts", "back.ts", "back.payload"]
                     , propsI = props ["sessional", "frontsFiltered"] }
       -->  emptyOut { attrsO = delAttrs }
    ]
  , CGraph.tfms2 = m
    [ "joinQuery"
      `ap2` emptyIn  { attrsI = attr "front.queryId" }
       <&>  emptyIn  { attrsI = attr "back.queryId" }
       -->  emptyOut { propsO = prop "joinedByUserQuery" }  -- TODO join property
    ]
  }

graph :: Graph
graph = undefined

graph' :: Graph
graph' = undefined
