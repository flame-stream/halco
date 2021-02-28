from typing import List, Any, Callable

from .conts import OutCont, InCont
from .graph import Graph
from .defs import Node, Stream, Tfm1, Tfm2, NodeType

IN1_CONT_ATTR = '__in1_cont__'
IN2_CONT_ATTR = '__in2_cont__'
OUT_CONT_ATTR = '__out_cont__'


class CGraph:
    def __init__(self, globals: Dict[str, Any], semantics: List[Node]):
        # TODO Не только dir, но и импорты надо инспектить
        self.streams = {name: node for name, node in globals.items() if is_stream(node)}
        self.tfms1 = {name: node for name, node in globals.items() if is_tfm1(node)}
        self.tfms2 = {name: node for name, node in globals.items() if is_tfm2(node)}
        self.semantics = {n.__name__ for n in semantics}

    def gen_graphs(self) -> List[Graph]:
        raise NotImplementedError  # TODO


def stream(o: OutCont) -> Callable[[Stream], [Stream]]:
    def decorator(f: Stream) -> Stream:
        setattr(f, OUT_CONT_ATTR, o)
        return f
    return decorator


def tfm1(i: InCont, o: OutCont) -> Callable[[Tfm1], [Tfm1]]:
    def decorator(f: Tfm1) -> Tfm1:
        setattr(f, IN1_CONT_ATTR, i)
        setattr(f, OUT_CONT_ATTR, o)
        return f
    return decorator


def tfm2(i1: InCont, i2: InCont, o: OutCont) -> Callable[[Tfm2], [Tfm2]]:
    def decorator(f: Tfm2) -> Tfm2:
        setattr(f, IN1_CONT_ATTR, i1)
        setattr(f, IN2_CONT_ATTR, i2)
        setattr(f, OUT_CONT_ATTR, o)
        return f
    return decorator


def is_stream(n: Node) -> bool:
    return node_type(n) == NodeType.STREAM


def is_tfm1(n: Node) -> bool:
    return node_type(n) == NodeType.TFM1


def is_tfm2(n: Node) -> bool:
    return node_type(n) == NodeType.TFM2


def node_type(n: Node) -> NodeType:
    if hasattr(n, IN2_CONT_ATTR):
        return NodeType.TFM2

    if hasattr(n, IN1_CONT_ATTR):
        return NodeType.TFM1

    return NodeType.STREAM


# # todo(frogofjuly): isinstance does not support generic types, so I ended up with this garbage


# def isStream(node: Node) -> bool:
#     return len(node) == 1


# def isTfm1(node: Node) -> bool:
#     return len(node) == 2


# def isTfm2(node: Node) -> bool:
#     return len(node) == 3


# def isTfm(node: Node) -> bool:
#     return not isStream(node)


# def env(env_list: PVector[Tuple[NodeName, Node]]) -> Env:
#     return pmap(env_list)


# def semantics(sem_list: PVector[NodeName]) -> Semantics:
#     return pset(sem_list)


# def envToList(env: Env) -> PVector[(NodeName, Node)]:
#     return PVector[env.items()]


# def nodeNamesP(f: Callable[[Node], bool], env: Env) -> PVector[NodeName]:
#     return pvector(map(lambda item: item[0], filter(lambda item: f(item[1]), envToList(env))))


# def streams(env: Env) -> PVector[NodeName]:
#     return nodeNamesP(isStream, env)


# def tfms(env: Env) -> PVector[NodeName]:
#     return nodeNamesP(isTfm, env)


# def tfms1(env: Env) -> PVector[NodeName]:
#     return nodeNamesP(isTfm1, env)


# def tfms2(env: Env) -> PVector[NodeName]:
#     return nodeNamesP(isTfm2, env)


# def stream(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
#     node = env[nn]
#     if not isStream(node):
#         raise RuntimeError(f"{nn} is supposed to be stream not {type(node)}")
#     return node


# def tfm1(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
#     node = env[nn]
#     if not isTfm1(node):
#         raise RuntimeError(f"{nn} is supposed to be tmf1 not {type(node)}")
#     return node


# def tfm2(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
#     node = env[nn]
#     if not isTfm2(node):
#         raise RuntimeError(f"{nn} is supposed to be tfm2 not {type(node)}")
#     return node


# def tfm1Conts(env: Env) -> PVector[Tuple[InCont, OutCont]]:
#     return pvector(map(lambda x: tfm1(env, x), tfms1(env)))


# def tfm2Conts(env: Env) -> PVector[Tuple[InCont, OutCont]]:
#     return pvector(map(lambda x: tfm2(env, x), tfms2(env)))
