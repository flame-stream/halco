module Calco.Cost where

import           Calco.Graph

-- Compile time information.
type CTInfo = ()
-- Runtime information.
type RTInfo = ()

cost :: Graph -> CTInfo -> RTInfo -> Integer
cost = undefined
