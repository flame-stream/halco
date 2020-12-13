module Examples.Pets.V1.CoGraph (cograph) where

import           Calco.CoGraph
import           Calco.Conts.General as G
import           Calco.DSL
import qualified Data.Map            as Map
import qualified Data.Set            as Set

cograph :: CoGraph
cograph = (env, semantics)

semantics :: Semantics
semantics = Set.fromList ["personStats", "stats", "breedsStats"]

env :: Env G.InCont G.OutCont
env = Env { streams = streams
          , tfms1 = tfms1
          , tfms2 = tfms2 }

streams :: Streams
streams = Map.fromList
  [ "pets" `ap0` emptyOut
    { attrsO = Set.fromList ["pet.id", "pet.name" "pet.age"] }
  , "persons" `ap0` emptyOut
    { attrs0 = Set.fromList ["person.id", "person.name", "person.age"] }
  ]

tfms1 :: Tfms1
tfms1 = undefined

tfms2 :: Tfms2
tfms2 = undefined
