{-# LANGUAGE TupleSections #-}

module Examples.Pets.Base (cograph) where

import qualified Data.Map         as Map
import qualified Data.Set         as Set

import           Calco.CGraph
import           Calco.Conts.Base
import           Calco.DSL

import           Examples.Pets

cograph :: CGraph InCont OutCont
cograph = (, petsSemantics) $ env
  [ "pets"    `ap0` outCont ["pet.id", "pet.name", "pet.age", "pet.breedId"] [] []
  , "persons" `ap0` outCont ["person.id", "person.name", "person.age"] [] []
  , "friends" `ap0` outCont ["personId", "petId"] [] []
  , "breeds"  `ap0` outCont ["breed.id", "breed.name"] [] []

  , "petNamesStats" `ap1` inCont ["pet.name"]
                                 []
                                 ["sameAge", "noDinosaurs"] -- To consider all pets
                     -->  outCont [] [] []

  , "priceNames" `ap1` inCont ["person.name", "pet.name"] ["sameAge", "noDinosaurs"] []
                  -->  outContN [] [] []

  , "nameBreedCorr" `ap1` inCont ["person.name", "breed.name"] ["noDinosaurs"] []
                     --> outContN [] [] []

  , "filterDinosaurs" `ap1` inCont ["pet.age"] [] []
                       -->  outCont [] ["noDinosaurs"] []

  , "filterSameAge" `ap1` inCont ["pet.age", "person.age"] [] []
                     -->  outCont [] ["sameAge"] []

  , "joinPetsFriends" `ap2` inCont ["pet.id"] [] []
                       <&>  inCont ["petId"] [] []
                       -->  outCont [] [] []

  , "joinPersonsFriends" `ap2` inCont ["person.id"] [] []
                          <&>  inCont ["personId"] [] []
                          -->  outCont [] [] []

  , "joinPetsBreeds" `ap2` inCont ["pet.breedId"] [] []
                      <&>  inCont ["breed.id"] [] []
                      -->  outCont [] [] []
  ]
