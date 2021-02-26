from typing import NewType, List, Union, Iterator, Optional
from dataclasses import dataclass
from pyrsistent import PSet

from .state import State


Attr = NewType('Attr', str)
Prop = NewType('Prop', str)


class Attrs:
    def __init__(self, name: str, attrs: List[Attr]):
        self.name = name
        self.attrs = attrs
        for attr in attrs:
            setattr(self, attr, name + attr)

    def __iter__(self) -> Iterator[Attr]:
        return iter(self.attrs)


class PropGroup:
    def __init__(self):
        self.props = set()

    def add(self, prop: Prop) -> None:
        self.props.add(prop)


def attr(attr: Attr) -> Attr:
    return attr


def prop(prop: Prop, groups: Union[PropGroup, List[PropGroup]]) -> Prop:
    if isinstance(groups, PropGroup):
        groups = [groups]

    for group in groups:
        group.add(prop)

    return prop


@dataclass
class InCont:
    attrs: PSet  # Needed attributes
    props: PSet  # Needed properties of the data
    noprops: PSet  # Prohibited properties

    def __init__(self, attrs=None, props=None, noprops=None):
        pass  # TODO

    def match(self, s: State) -> bool:
        return (self.attrs.issubset(s.attrs) and
                self.props.issubset(s.props) and
                self.noprops.isdisjoint(s.props))


@dataclass
class OutCont:
    attrs: PSet  # Attributes that will be added
    rem: bool  # Will old attributes be removed
    props: PSet  # Properties that will be added
    remprops: PSet  # Properties to be removed

    def __init__(
        self,
        attrs: Optional[Union[Attrs, Set[Attr]]] = None,
        rem=False,
        props=None,
        remprops=None
    ):
        pass  # TODO

    def update(self, s: State) -> State:
        return State(
            attrs=self.attrs if self.rem else self.attrs | s.attrs,
            props=self.props | (s.props - self.remprops)
        )
