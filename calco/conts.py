from dataclasses import dataclass
# from typing import Set
from pyrsistent import PSet

from .defs import NodeName, Attr, Prop
from .state import State


@dataclass
class InCont:
    attrs: PSet[Attr]
    props: PSet[Prop]
    propsp: PSet[Prop]  # Prohibited props

    def match(self, s: State) -> bool:
        return (self.attrs.issubset(s.attrs) and
                self.props.issubset(s.props) and
                self.propsp.isdisjoint(s.props))


@dataclass
class OutCont:
    attrs: PSet[Attr]
    rem: bool  # Will old attributes be removed
    props: PSet[Prop]
    propsr: PSet[Prop]  # Props to be removed

    def update(self, s: State) -> State:
        return State(
            attrs=self.attrs if self.rem else self.attrs | s.attrs,
            props=self.props | (s.props - self.propsr)
        )
