{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Data.Function                ((&))
import           Data.Map                     (Map, (!))
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Calco.CGraph                 (Semantics)
import           Calco.Defs                   (NodeName)
import           Calco.Utils.Data.List        (cartesianProduct)
import           Calco.Utils.Data.Map         (findKeys)
import           Calco.Utils.Data.Traversable (countOccs)

type NodeId = Integer

-- Set of nids that have terms in the graph that form its semantics.
type SemanticNids = Set NodeId

data Node =
    Stream NodeName
  | Tfm1 NodeName NodeId
  | Tfm2 NodeName NodeId NodeId
  deriving (Ord, Eq, Show)

newtype Graph = Graph (Map NodeId Node)

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

empty :: Graph
empty = graph []

union :: Graph -> Graph -> Graph
union (Graph m1) (Graph m2) = Graph $ m1 <> m2

nodeName :: Node -> NodeName
nodeName = \case
  Stream nn   -> nn
  Tfm1 nn _   -> nn
  Tfm2 nn _ _ -> nn

nodeNames :: Graph -> [NodeName]
nodeNames (Graph m) = map nodeName $ Map.elems m

-- Extracts graph that has at least one of each nid of semantics.
extractPipeline :: Graph -> SemanticNids -> Graph
extractPipeline g nids = Map.foldrWithKey f empty $ toMap g
  where
    f :: NodeId -> Node -> Graph -> Graph
    f nid _ g'@(Graph m')
      | nid `Map.member` m' = g'
      | nid `Set.notMember` nids = g'
      | otherwise = extractPipeline' g nid g'

    extractPipeline' :: Graph -> NodeId -> Graph -> Graph
    extractPipeline' g@(Graph m) nid g'@(Graph m')
      | nid `Map.member` m' = g'
      | otherwise = m ! nid & Graph . \case
        s@(Stream _) -> Map.insert nid s m'
        t@(Tfm1 _ nid') -> Map.insert nid t . toMap $ extractPipeline' g nid' g'
        t@(Tfm2 _ nid1 nid2) ->
          let m1 = toMap $ extractPipeline' g nid1 g'
              m2 = toMap $ extractPipeline' g nid2 g'
          in Map.insert nid t $ m1 <> m2

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
  let prefix = "digraph " ++ "\"" ++ name ++ "\"" ++" {\n"
      -- split by \n only if appending non-empty string (x)
      delim = (\ x y -> y ++ if x /= "" then "\n" ++ x else "")
      vtxes = foldr (delim . (\x -> "\"" ++ x ++ "\"")) "" (nodeNames g)
      strGraph = foldr (delim . edge2dot) "" (toList g)
      suffix = "\n}\n"
   in prefix ++ vtxes ++ strGraph ++ suffix
  where
    edge2dot :: (NodeId, Node) -> String
    edge2dot (id, Stream nn) = ""
    edge2dot (id, Tfm1 nn nid) = nameLookup nid  ++ " -> " ++ "\"" ++ nn ++ "\""
    edge2dot (id, Tfm2 nn nid1 nid2) =
      let edge1 = nameLookup nid1 ++ " -> " ++ "\"" ++ nn ++ "\""
          edge2 = nameLookup nid2 ++ " -> " ++ "\"" ++ nn ++ "\""
       in edge1 ++ "\n" ++ edge2

    nameLookup :: NodeId -> String
    nameLookup id = "\"" ++ nodeName (Map.findWithDefault (Stream "undefined id") id (toMap g)) ++ "\""
