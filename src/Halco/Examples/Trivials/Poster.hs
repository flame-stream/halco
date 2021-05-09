{-# LANGUAGE TupleSections #-}

module Halco.Examples.Trivials.Poster where

import           Data.Map                       (Map, (!))
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set

import           Halco.CGraph                   (CGraph, Env (Env), Semantics)
import qualified Halco.CGraph                   as CGraph
import           Halco.Combs                    (coReduceNode', pardoNodeP,
                                                 reduceNode)
import           Halco.ContImpls.Trivial.Conts
import qualified Halco.ContImpls.Trivial.Conts  as Trivial
import           Halco.ContImpls.Trivial.DSL
import           Halco.ContImpls.Trivial.State  (State)
import           Halco.ContImpls.Trivials.Conts
import qualified Halco.ContImpls.Trivials.Conts as Trivials
import           Halco.Conts
import           Halco.DSL
import           Halco.EGraph
import           Halco.Graph                    (Graph, Node (..), graph)
import           Halco.Utils.Classes            (Empty (..))

semantics ::Semantics
semantics = s ["fraudMetric", "participants"]

cgraph :: CGraph State Trivials.InCont Trivial.OutCont Trivial.OutCont Trivial.OutCont
cgraph = (, semantics) $ Env
  { CGraph.sources = m
    [ "bid" `ap0` empty
      { attrsO = NewAttrs $ attrs' "bid" ["id", "bidder", "auction", "price", "dateTime"] }
    , "auction" `ap0` empty
      { attrsO = NewAttrs $ attrs' "auction" ["id", "itemName", "category"] }
    , "person" `ap0` empty
      { attrsO = NewAttrs $ attrs' "person" ["id", "name", "dateTime"] }
    ]
  , CGraph.ops1 = m
    [ "fraudMetric"
      `ap1` Trivials.InCont
            [ empty { attrsI = attr "bid.id" }
            , empty { attrsI = attrs ["bid.id", "person.id"], propsI = prop "joinBP" }
            , empty { attrsI = attrs ["bid.id", "auction.id"], propsI = prop "joinBA" }
            , empty { attrsI = attrs ["bid.id", "person.id", "auction.id"]
                    , propsI = props ["joinBP", "joinBA"] } ]
       -->  empty { attrsO = delAttrs }
    , "participants"
      `ap1` Trivials.InCont [
            empty { attrsI = attrs ["auction.id", "person.id"]
                  , propsI = props ["joinBP", "joinBA"] } ]
       -->  empty { attrsO = delAttrs }
    ]
  , CGraph.ops2 = m
    [ "joinBP"
      `ap2` Trivials.InCont [empty { attrsI = attr "bid.bidder" }]
       <&>  Trivials.InCont [empty { attrsI = attr "person.id" }]
       -->  empty { propsO = prop "joinBP" }
    , "joinBA"
      `ap2` Trivials.InCont [empty { attrsI = attr "bid.auction" }]
       <&>  Trivials.InCont [empty { attrsI = attr "auction.id" }]
       -->  empty { propsO = prop "joinBA" }
    ]
  }
