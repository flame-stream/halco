module Halco.Cost where

import           Halco.Graph

-- Compile time information.
type CTInfo = ()
-- Runtime information.
type RTInfo = ()

cost :: CTInfo -> Maybe RTInfo
     -> Graph -> Integer
cost = undefined
