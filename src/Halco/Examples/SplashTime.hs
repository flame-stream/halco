{-# LANGUAGE TupleSections #-}

module Halco.Examples.SplashTime where

import           Data.Map          (Map)

import           Halco.CGraph      (CGraph, Env (Env), Semantics)
import qualified Halco.CGraph      as CGraph
import           Halco.Conts.Impl
import qualified Halco.Conts.Impl  as Impl
import           Halco.Conts.Types
import           Halco.DSL
import           Halco.EGraph
import           Halco.Graph

semantics :: Semantics
semantics = s ["stats"]

cgraph :: CGraph Impl.InCont Impl.OutCont
cgraph = (, semantics) $ Env
  { CGraph.sources = m
    [ "frontLogs" `ap0` emptyOut
      { attrsO = NewAttrs $ attrs' "front" ["id", "version", "queryId", "userId", "ts"] }

    , "backLogs" `ap0` emptyOut
      { attrsO = NewAttrs $ attrs' "back" ["id", "queryId", "userId", "ts", "payload"] }
    ]
  , CGraph.tfms1 = m
    [ "addFrontFeatures"
      `ap1` emptyIn  { attrsI = attr "front.version" }
       -->  emptyOut { attrsO = AddAttrs $ attr "frontFeatures" }

    , "addUserFeatures"
      `ap1` emptyIn  { attrsI  = attr "front.userId"
                     , propsI' = prop "authorizedUsers" }
       -->  emptyOut { attrsO  = AddAttrs $ attr "userFeatures" }

    , "setSessionTrigger"
      `ap1` emptyIn  { attrsI  = attrs [ "front.userId", "frontFeatures"
                                       , "userFeatures", "front.ts", "back.ts"]
                     , propsI  = prop "frontsFiltered"
                     , propsI' = prop "authorizedUsers" }
       -->  emptyOut { attrsO  = AddAttrs $ attr "sessionEnd" }

    , "filterUsers"
      `ap1` emptyIn  { attrsI = attr "userFeatures" }
       -->  emptyOut { propsO = prop "authorizedUsers" }

    , "filterFronts"
      `ap1` emptyIn  { attrsI = attr "front.version" }
       -->  emptyOut { propsO = prop "frontsFiltered" }

    , "stats"
      `ap1` emptyIn  { attrsI = attrs [ "front.userId", "front.ts", "back.ts"
                                      , "back.payload", "sessionEnd"]
                     , propsI = props ["frontsFiltered", "authorizedUsers"] }
       -->  emptyOut { attrsO = delAttrs }
    ]
  , CGraph.tfms2 = m
    [ "joinByQuery"
      `ap2` emptyIn  { attrsI = attr "front.queryId" }
       <&>  emptyIn  { attrsI = attr "back.queryId" }
       -->  emptyOut { propsO = prop "joinedByUserQuery" }  -- TODO join property
    ]
  }

graph :: Graph
graph = undefined

graph' :: Graph
graph' = undefined
