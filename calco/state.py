from typing import Iterable, Optional
from pyrsistent import PSet, PClass, pset_field, s

from .defs import Attr, Prop


# TODO maybe chage to dataclass.
class State(PClass):
    attrs = pset_field(Attr)
    props = pset_field(Prop)

    def __init__(
        self,
        attrs: Optional[Iterable[Attr]] = None,
        props: Optional[Iterable[Prop]] = None,
    ):
        self.attrs = s() if attrs is None else PSet(attrs)
        self.props = s() if props is None else PSet(props)

    def __or__(self, s: State):
        return State(self.attrs | s.attrs, self.props | s.props)
