module Halco.State where

import           Halco.Utils.Classes (Empty)

-- Information about the data
class (Empty s, Show s) => State s
