module Halco.ContImpls.Trivial.DSL where

import           Data.Set                      (Set)
import qualified Data.Set                      as Set

import           Halco.ContImpls.Trivial.Conts (OutAttrs (NewAttrs))
import           Halco.ContImpls.Trivial.Defs  (Attr (..), Prop (..))
import           Halco.Defs                    (NodeName)

attr :: NodeName -> Set Attr
attr = Set.singleton . Attr

attrs :: [NodeName] -> Set Attr
attrs =  Set.fromList . map Attr

attrs' :: NodeName -> [NodeName] -> Set Attr
attrs' prefix = Set.fromList . map (Attr . (prefix ++) . ('.' :))

prop :: NodeName -> Set Prop
prop = Set.singleton . Prop

props :: [NodeName] -> Set Prop
props = Set.fromList . map Prop

delAttrs :: OutAttrs
delAttrs = NewAttrs Set.empty
