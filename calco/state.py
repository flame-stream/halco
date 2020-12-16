from dataclasses import dataclass
from .defs import Attr, Prop
from typing import Set


@dataclass
class State:
    attrs: Set[Attr]
    props: Set[Prop]

    def __init__(self, attrs=None, props=None):
        self.attrs = {} if attrs is None else attrs
        self.props = {} if props is None else props

    def __ior__(self, s: State):
        return State(self.attrs | s.attrs, self.props | s.props)
