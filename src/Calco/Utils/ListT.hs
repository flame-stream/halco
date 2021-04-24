module Calco.Utils.ListT where

import           ListT

nilLT :: Monad m => ListT m a
nilLT = ListT $ pure Nothing
