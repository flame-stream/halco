module Halco.Scheme where

import           Halco.Utils.Classes (Empty)

-- Information about the data
class (Empty s, Show s) => Scheme s
