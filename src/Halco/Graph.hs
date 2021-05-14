{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Halco.Graph where

import           Data.Function                ((&))
import           Data.Map                     (Map, (!))
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Halco.CGraph                 (Semantics)
import           Halco.Defs                   (NodeName)
import           Halco.Utils.Classes          (Empty (..))
import           Halco.Utils.Data.List        (cartesianProduct)
import           Halco.Utils.Data.Map         (findKeys)
import           Halco.Utils.Data.Traversable (countOccs)

type NodeId = Integer

-- Set of nids of the graph nodes that form its semantics
type SemanticNids = Set NodeId

data Node =
    Source NodeName
  | Op1 NodeName NodeId
  | Op2 NodeName NodeId NodeId
  deriving (Ord, Eq, Show)

newtype Graph = Graph (Map NodeId Node)

instance Empty Graph where
  empty = graph []

instance Semigroup Graph where
  (<>) = union

instance Monoid Graph where
  mempty = empty

instance Show Graph where
  show = flip graph2Dot "graph"

graph :: [(NodeId, Node)] -> Graph
graph = Graph . Map.fromList

toMap :: Graph -> Map NodeId Node
toMap (Graph m) = m

toList :: Graph -> [(NodeId, Node)]
toList = Map.toList . toMap

union :: Graph -> Graph -> Graph
union (Graph m1) (Graph m2) = Graph $ m1 <> m2

nodeName :: Node -> NodeName
nodeName = \case
  Source nn  -> nn
  Op1 nn _   -> nn
  Op2 nn _ _ -> nn

nodeNames :: Graph -> [NodeName]
nodeNames (Graph m) = map nodeName $ Map.elems m

-- Extracts graph that has at least one of each node of semantics in its leafs
extractDataflow :: Graph -> SemanticNids -> Graph
extractDataflow g nids = Map.foldrWithKey f empty $ toMap g
  where
    f :: NodeId -> Node -> Graph -> Graph
    f nid _ g'@(Graph m')
      | nid `Map.member` m' = g'
      | nid `Set.notMember` nids = g'
      | otherwise = extractDataflow' g nid g'

    extractDataflow' :: Graph -> NodeId -> Graph -> Graph
    extractDataflow' g@(Graph m) nid g'@(Graph m')
      | nid `Map.member` m' = g'
      | otherwise = m ! nid & Graph . \case
        s@(Source _) -> Map.insert nid s m'
        o@(Op1 _ nid') -> Map.insert nid o . toMap $ extractDataflow' g nid' g'
        o@(Op2 _ nid1 nid2) ->
          let m1 = toMap $ extractDataflow' g nid1 g' in
          let m2 = toMap $ extractDataflow' g nid2 g' in
          Map.insert nid o $ m1 <> m2

findIds :: Graph -> NodeName -> [NodeId]
findIds (Graph m) nn = findKeys nn $ nodeName <$> m

semanticNids :: Graph -> Semantics -> [SemanticNids]
semanticNids g = (Set.fromList <$>)
                . cartesianProduct . (findIds g <$>)
                . Set.toList

noSameNodes :: Graph -> Bool
noSameNodes = all (== (1 :: Integer)) . countOccs . nodeNames

graph2Dot :: Graph -> String -> String
graph2Dot g name =
  let prefix = "digraph " ++ "\"" ++ name ++ "\"" ++" {\n" in
  -- split by \n only if appending non-empty string (x)
  let delim = (\ x y -> y ++ if x /= "" then "\n" ++ x else "") in
  let vtxes = foldr (delim . (\x -> "\"" ++ x ++ "\"")) "" (nodeNames g) in
  let strGraph = foldr (delim . edge2dot) "" (toList g) in
  let suffix = "\n}\n" in
  prefix ++ vtxes ++ strGraph ++ suffix
  where
    edge2dot :: (NodeId, Node) -> String
    edge2dot (id, Source nn) = ""
    edge2dot (id, Op1 nn nid) = nameLookup nid  ++ " -> " ++ "\"" ++ nn ++ "\""
    edge2dot (id, Op2 nn nid1 nid2) =
      let edge1 = nameLookup nid1 ++ " -> " ++ "\"" ++ nn ++ "\"" in
      let edge2 = nameLookup nid2 ++ " -> " ++ "\"" ++ nn ++ "\"" in
      edge1 ++ "\n" ++ edge2

    nameLookup :: NodeId -> String
    nameLookup id = "\"" ++ nodeName (Map.findWithDefault (Source "undefined id") id (toMap g)) ++ "\""
