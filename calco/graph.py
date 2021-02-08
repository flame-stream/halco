# from typing import Dict, Tuple, Union, Set, List
from typing import Tuple, Union
from pyrsistent import PMap, PVector, PSet
from pyrsistent import pmap, pvector, pset
from .defs import NodeName
from .cgraph import Semantics
from itertools import product

TermId = int

SemanticTids = PSet[TermId]

Const = Tuple[NodeName]
App1 = Tuple[NodeName, TermId]
App2 = Tuple[NodeName, TermId, TermId]
Term = Union[Const, App1, App2]

Graph = PMap[TermId, Term]


# todo(frogofjuly): isinstance does not support generic types, so I ended up with this garbage

def isConst(t: Term) -> bool:
    return len(t) == 1


def isApp1(t: Term) -> bool:
    return len(t) == 2


def isApp2(t: Term) -> bool:
    return len(t) == 3


def graph(glist: PVector[Tuple[TermId, Term]]) -> Graph:
    return pmap(glist)


def toList(g: Graph) -> PVector[Tuple[TermId, Term]]:
    return pvector(g.items())


def empty() -> Graph:
    return pmap()


def union(g1: Graph, g2: Graph) -> Graph:
    return g1 + g2


def nodeName(t: Term) -> NodeName:
    return t[0]


def nodeNames(g: Graph) -> PVector[NodeName]:
    return pvector(map(NodeName, g.values()))


def extractPipline(g: Graph, sem: SemanticTids) -> Graph:
    def extractPiplineHelper(g: Graph, tid: TermId) -> Graph:
        t = g[tid]
        if isConst(t):
            return pmap({tid: t})

        if isApp1(t):
            tid_prime: TermId = t[1]
            g_prime = extractPiplineHelper(g, tid_prime)
            return g_prime + pmap({tid: t})

        if isApp2(t):
            tid1: TermId = t[1]
            tid2: TermId = t[2]
            g1 = extractPiplineHelper(g, tid1)
            g2 = extractPiplineHelper(g, tid2)
            return g1 + g2 + pmap({tid: t})

        raise RuntimeError(f"This is some very unexpected shit right here: {t}."
                           f" Expected term, got: '{type(t)}' instead")

    def f(tid: TermId, _: Term, g: Graph) -> Graph:
        if tid in g:
            return g
        if tid not in sem:
            return g
        return extractPiplineHelper(g, tid) + g

    g_res = empty()
    for item in g.items():
        tid: TermId = item[0]
        t: Term = item[1]
        g_res = f(tid, t, g_res)

    return g_res


def findIds(g: Graph, nn: NodeName) -> PVector[NodeName]:
    res = pvector()
    for item in g.items():
        _: TermId = item[0]
        t: Term = item[1]
        if nn == nodeName(t):
            res = res.append(nn)
    return res


def semanticsTids(g: Graph, sem: Semantics) -> [SemanticTids]:
    return pset(
        product(
            [
                findIds(g, nn) for nn in sem
            ]
        )
    )


def noSameNodes(g: Graph) -> bool:
    names = nodeNames(g)
    return len(pset(names)) == len(names)


def graph2Dot(g: Graph, name: str) -> str:
    def nameLookup(tid: TermId) -> str:
        return "\"" + nodeName(g[tid]) + "\""

    def edge2Dot(tid: TermId, t: Term) -> str:
        nn = "\"" + t[0] + "\""
        if isConst(t):
            return ""
        if isApp1(t):
            inputId: TermId = t[1]
            return nameLookup(inputId) + " -> " + nn

        if isApp2(t):
            inputId1: TermId = t[1]
            inputId2: TermId = t[2]
            return nameLookup(inputId1) + " -> " + nn + "\n" + \
                   nameLookup(inputId2) + " -> " + nn

        raise RuntimeError(f"This is some very unexpected shit right here: {t}."
                           f" Expected term, got: '{type(t)}' instead")

    prefix = "digraph " + "\"" + name + "\"" + " {\n"
    suffix = "\n}\n"
    vtxes = ""
    for nn in nodeNames(g):
        vtxes += "\"" + nn + "\"\n"

    edges = ""
    for item in g.items():
        tid: TermId = item[0]
        t: Term = item[1]
        str_edge = edge2Dot(tid, t)
        if str_edge == "":
            continue

        edges += str_edge

    return prefix + vtxes + edges + suffix
