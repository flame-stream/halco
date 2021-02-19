from dataclasses import dataclass
from pyrsistent import PSet

from .defs import Attr, Prop


@dataclass
class State:
    attrs: PSet
    props: PSet

    def __init__(self, attrs=None, props=None):
        self.attrs = {} if attrs is None else attrs
        self.props = {} if props is None else props

    def union(self, s: State):
        return State(self.attrs.union(s.attrs), self.props.union(s.props))
