from .cgraph import CGraph, Env, Semantics
from .graph import Graph, TermId, Term, isConst, isApp1, isApp2
from .state import State
from typing import Tuple
from pyrsistent import PSet, PMap, PVector
from pyrsistent import pset, pmap, pvector

CheckedTerms = PMap[TermId, State]


def check(cograph: CGraph, graph: Graph) -> bool:
    env, semantics = cograph
    return hasSemantics(semantics, graph) and checkTerms(env, graph)


def hasSemantics(semantics: Semantics, graph: Graph) -> bool:
    g_sem = pset()
    for item in graph.items():
        _: TermId = item[0]
        t: Term = item[1]
        g_sem = g_sem.add(t[0])
    return semantics.issubset(g_sem)


def checkTerms(env: Env, graph: Graph) -> bool:
    raise NotImplementedError  # TODO


def checkTerm(env: Env, g: Graph,
              chcked: CheckedTerms,
              tid: TermId) -> CheckedTerms:
    pass


def checkTermHelper(env: Env, g: Graph,
                    chcked: CheckedTerms,
                    tid: TermId) -> Tuple[State, CheckedTerms]:
    if tid in chcked:
        return chcked[tid], chcked

    t: Term = g[tid]

    if isConst(t):
        pass

    if isApp1(t):
        pass

    if isApp2(t):
        pass
