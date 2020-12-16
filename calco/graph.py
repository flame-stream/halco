from typing import Dict, Tuple, Union
from .defs import NodeName


TermMarker = int

Const = Tuple[NodeName]
App1 = Tuple[NodeName, TermMarker]
App2 = Tuple[NodeName, TermMarker, TermMarker]
Term = Union[Const, App1, App2]

Graph = Dict[TermMarker, Term]
