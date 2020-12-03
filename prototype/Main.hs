module Main where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (isJust)


type StreamName = String
type AttrName = String
type PropName = String

data Attr = Attr StreamName AttrName
  deriving (Show, Ord, Eq)

data Prop = Prop PropName | AttrProp Attr PropName
  deriving (Show, Ord, Eq)


data InCont = InCont { attrsI :: Set Attr
                     , propsI :: Set Prop
                     , propsI' :: Set Prop }
  deriving (Show)

emptyInCont :: InCont
emptyInCont = InCont { attrsI = Set.empty
                     , propsI = Set.empty
                     , propsI' = Set.empty }


data DelAttrs = DelAllAttrs | DelAttrs (Set Attr)
  deriving (Show)

data OutCont = OutCont { attrsO :: Set Attr
                       , attrsO' :: DelAttrs
                       , propsO :: Set Prop
                       , propsO' :: Set Prop }
  deriving (Show)

emptyOutCont :: OutCont
emptyOutCont = OutCont { attrsO = Set.empty
                       , attrsO' = DelAttrs Set.empty
                       , propsO = Set.empty
                       , propsO' = Set.empty }


-- Transform name
type TfmName = String

data Term = Const StreamName
          | App1 TfmName Term
          | App2 TfmName Term Term
  deriving (Show)

type Graph = Set Term

type Streams = Map StreamName OutCont
type Tfms1 = Map TfmName (InCont, OutCont)
type Tfms2 = Map TfmName (InCont, InCont, OutCont)
type Result = Set TfmName

data Env = Env { streams :: Streams
               , tfms1 :: Tfms1
               , tfms2 :: Tfms2 }
  deriving (Show)


-- Stream state after transformations
data State = State { attrs :: Set Attr
                   , props :: Set Prop }
  deriving (Show)

emptyState :: State
emptyState = State { attrs = Set.empty
                   , props = Set.empty }


checkTerm :: Env -> Term -> Maybe State
checkTerm e t = case t of
  Const c -> updateState emptyState <$> Map.lookup c (e & streams)
  App1 f t' -> do
    s <- checkTerm e t'
    (inCont, outCont) <- Map.lookup f (e & tfms1)
    if matchState s inCont
      then Just $ updateState s outCont
      else Nothing
  App2 f t1 t2 -> do
    s1 <- checkTerm e t1
    s2 <- checkTerm e t2
    (inCont1, inCont2, outCont) <- Map.lookup f (e & tfms2)
    if matchState s1 inCont1 && matchState s2 inCont2
      then Just $ updateState (unionState s1 s2) outCont
      else Nothing

checkTerms :: Traversable t => Env -> t Term -> Bool
checkTerms e = all (isJust . checkTerm e)

updateState :: State -> OutCont -> State
updateState s c =
  let projected = case c & attrsO' of
                    DelAllAttrs -> Set.empty
                    DelAttrs attrs' -> (s & attrs) \\ attrs' in
  State { attrs = Set.union (c & attrsO) projected
        , props = Set.union (c & propsO) $ (s & props) \\ (c & propsO') }

matchState :: State -> InCont -> Bool
matchState s c = (c & attrsI) `Set.isSubsetOf` (s & attrs)
              && (c & propsI) `Set.isSubsetOf` (s & props)
              && (c & propsI') `Set.disjoint`  (s & props)

unionState :: State -> State -> State
unionState s1 s2 = State { attrs = (s1 & attrs) `Set.union` (s2 & attrs)
                         , props = (s1 & props) `Set.union` (s2 & props) }

generate :: Env -> Result -> [Term]
generate = undefined

cost :: Term -> Integer
cost = undefined

examplePets :: Maybe State
examplePets = checkTerm e t
  where
    petsAttrs = map (\(Attr _ a) -> Attr "pets" a)
                    [Attr "" "id", Attr "" "name", Attr "" "age"]
    petsCont = emptyOutCont { attrsO = Set.fromList petsAttrs }

    personsAttrs = map (\(Attr _ a) -> Attr "persons" a)
                       [Attr "" "id", Attr "" "name", Attr "" "age"]
    personsProps = [AttrProp (personsAttrs !! 2) "ageLT10"]
    personsCont = emptyOutCont { attrsO = Set.fromList personsAttrs
                               , propsO = Set.fromList personsProps }

    e = Env { streams = Map.fromList [ ("pets", petsCont)
                                     , ("persons", personsCont) ]
            , tfms1 = undefined
            , tfms2 = undefined }

    t = undefined

main :: IO ()
main = undefined
