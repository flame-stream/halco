module Halco.State where

import           Halco.Utils.Classes (Empty)

class (Empty s, Show s) => State s
