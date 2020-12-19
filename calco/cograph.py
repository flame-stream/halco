from typing import Set, Tuple, Union, Dict, Callable
from .defs import NodeName


Stream = Tuple[OutCont]
Tfm1 = Tuple[InCont, OutCont]
Tfm2 = Tuple[InCont, InCont, OutCont]
Node = Union[Stream, Tfm1, Tfm2]

Env = Dict[NodeName, Node]

Semancis = Set[NodeName]

CoGraph = Tuple[Env, Semancis]


def cograph(semantics: Semancis) -> Tuple[CoGraph, Callable]:
    pass
