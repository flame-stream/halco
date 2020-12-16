from dataclasses import dataclass
from typing import Set

from .defs import NodeName, Attr, Prop
from .state import State


@dataclass
class InCont:
    attrs: Set[Attr]
    props: Set[Prop]
    propsp: Set[Prop]  # Prohibited props

    def match(self, s: State) -> bool:
        return (self.attrs.issubset(s.attrs) and
                self.props.issubset(s.props) and
                self.propsp.isdisjoint(s.props))


@dataclass
class OutCont:
    attrs: Set[Attr]
    rem: bool  # Will old attributes be removed
    props: Set[Prop]
    propsr: Set[Prop]  # Props to be removed

    def update(self, s: State) -> State:
        return State(
            attrs=self.attrs if self.rem else self.attrs | s.attrs,
            props=self.props | (s.props - c.propsr)
        )
