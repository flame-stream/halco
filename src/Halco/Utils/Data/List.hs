module Halco.Utils.Data.List where

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = []
cartesianProduct [xs]     = (: []) <$> xs
cartesianProduct (xs:xss) = (:) <$> xs <*> cartesianProduct xss

length' :: [a] -> Integer
length' = toInteger . length
