import           Calco.Utils

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- examplePets :: Maybe State
-- examplePets = checkTerm e t
--   where
--     petsAttrs = map (\(Attr _ a) -> Attr "pets" a)
--                     [Attr "" "id", Attr "" "name", Attr "" "age"]
--     petsCont = emptyOutCont { attrsO = Set.fromList petsAttrs }

--     personsAttrs = map (\(Attr _ a) -> Attr "persons" a)
--                        [Attr "" "id", Attr "" "name", Attr "" "age"]
--     personsProps = [AttrProp (personsAttrs !! 2) "ageLT10"]
--     personsCont = emptyOutCont { attrsO = Set.fromList personsAttrs
--                                , propsO = Set.fromList personsProps }

--     e = Env { streams = Map.fromList [ ("pets", petsCont)
--                                      , ("persons", personsCont) ]
--             , tfms1 = undefined
--             , tfms2 = undefined }

--     t = undefined
