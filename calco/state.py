from __future__ import annotations

from dataclasses import dataclass
from .defs import Attr, Prop
from pyrsistent import PSet


@dataclass
class State:
    attrs: PSet[Attr]
    props: PSet[Prop]

    def __init__(self, attrs=None, props=None):
        self.attrs = {} if attrs is None else attrs
        self.props = {} if props is None else props

    def union(self, s: State):
        return State(self.attrs.union(s.attrs), self.props.union(s.props))
