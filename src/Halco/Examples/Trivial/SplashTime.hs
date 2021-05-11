{-# LANGUAGE TupleSections #-}

module Halco.Examples.Trivial.SplashTime where

import           Data.Map                      (Map)

import           Halco.Beam.Combs              (coReduceNode', pardoNodeP,
                                                reduceNode)
import           Halco.CGraph                  (CGraph, Env (Env), Semantics)
import qualified Halco.CGraph                  as CGraph
import           Halco.ContImpls.Trivial.Conts
import qualified Halco.ContImpls.Trivial.Conts as Trivial
import           Halco.ContImpls.Trivial.DSL
import           Halco.ContImpls.Trivial.State (State)
import           Halco.Conts
import           Halco.DSL
import           Halco.EGraph
import           Halco.Graph                   (Graph, Node (..), graph)
import           Halco.Utils.Classes           (Empty (..))

semantics :: Semantics
semantics = s ["stats"]

cgraph :: CGraph State Trivial.InCont Trivial.OutCont Trivial.OutCont Trivial.OutCont
cgraph = (, semantics) $ Env
  { CGraph.sources = m
    [ "frontLogs" `ap0` empty
      { attrsO = NewAttrs $ attrs' "front" ["id", "version", "queryId", "userId", "ts"] }

    , "backLogs" `ap0` empty
      { attrsO = NewAttrs $ attrs' "back" ["id", "queryId", "userId", "ts", "payload"] }
    ]
  , CGraph.ops1 = m
    [ "addFrontFeatures"
      `ap1` empty { attrsI = attr "front.version" }
       -->  empty { attrsO = AddAttrs $ attr "frontFeatures" }

    , "addUserFeatures"
      `ap1` empty { attrsI  = attr "front.userId"
                  , propsI' = prop "authorizedUsers" }
       -->  empty { attrsO  = AddAttrs $ attr "userFeatures" }

    , "setSessionTrigger"
      `ap1` empty { attrsI  = attrs [ "front.userId", "frontFeatures"
                                    , "userFeatures", "front.ts", "back.ts"]
                  , propsI  = prop "frontsFiltered"
                  , propsI' = prop "authorizedUsers" }
       -->  empty { attrsO  = AddAttrs $ attr "sessionEnd" }

    , "filterUsers"
      `ap1` empty { attrsI = attr "userFeatures" }
       -->  empty { propsO = prop "authorizedUsers" }

    , "filterFronts"
      `ap1` empty { attrsI = attr "front.version" }
       -->  empty { propsO = prop "frontsFiltered" }

    , "stats"
      `ap1` empty { attrsI = attrs [ "front.userId", "front.ts", "back.ts"
                                   , "back.payload", "sessionEnd"]
                  , propsI = props ["frontsFiltered", "authorizedUsers"] }
       -->  empty { attrsO = delAttrs }
    ]
  , CGraph.ops2 = m
    [ "joinByQuery"
      `ap2` empty { attrsI = attr "front.queryId" }
       <&>  empty { attrsI = attr "back.queryId" }
       -->  empty { propsO = prop "joinedByUserQuery" }  -- TODO join property
    ]
  }

graph :: Graph
graph = undefined

graph' :: Graph
graph' = undefined
