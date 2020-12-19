from .cograph import CoGraph, Graph, Env, Semancis


def check(cograph: CoGraph, graph: Graph) -> bool:
    env, semantics = cograph
    return hasSemantics(semantics, graph) and checkTerms(env, graph)


def hasSemantics(semantics: Semantics, graph: Graph) -> bool:
    raise NotImplementedError  # TODO


def checkTerms(env: Env, graph: Graph) -> bool:
    raise NotImplementedError  # TODO
