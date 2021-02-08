from typing import Tuple, Union, Callable
from pyrsistent import PMap, PSet, PVector
from pyrsistent import pmap, pset, pvector
from .defs import NodeName
from .conts import OutCont, InCont

Stream = Tuple[OutCont]
Tfm1 = Tuple[InCont, OutCont]
Tfm2 = Tuple[InCont, InCont, OutCont]
Node = Union[Stream, Tfm1, Tfm2]

Env = PMap[NodeName, Node]

Semantics = PSet[NodeName]

CGraph = Tuple[Env, Semantics]


# todo(frogofjuly): isinstance does not support generic types, so I ended up with this garbage


def isStream(node: Node) -> bool:
    return len(node) == 1


def isTfm1(node: Node) -> bool:
    return len(node) == 2


def isTfm2(node: Node) -> bool:
    return len(node) == 3


def isTfm(node: Node) -> bool:
    return not isStream(node)


def env(env_list: PVector[Tuple[NodeName, Node]]) -> Env:
    return pmap(env_list)


def semantics(sem_list: PVector[NodeName]) -> Semantics:
    return pset(sem_list)


def envToList(env: Env) -> PVector[(NodeName, Node)]:
    return PVector[env.items()]


def nodeNamesP(f: Callable[[Node], bool], env: Env) -> PVector[NodeName]:
    return pvector(map(lambda item: item[0], filter(lambda item: f(item[1]), envToList(env))))


def streams(env: Env) -> PVector[NodeName]:
    return nodeNamesP(isStream, env)


def tfms(env: Env) -> PVector[NodeName]:
    return nodeNamesP(isTfm, env)


def tfms1(env: Env) -> PVector[NodeName]:
    return nodeNamesP(isTfm1, env)


def tfms2(env: Env) -> PVector[NodeName]:
    return nodeNamesP(isTfm2, env)


def stream(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
    node = env[nn]
    if not isStream(node):
        raise RuntimeError(f"{nn} is supposed to be stream not {type(node)}")
    return node


def tfm1(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
    node = env[nn]
    if not isTfm1(node):
        raise RuntimeError(f"{nn} is supposed to be tmf1 not {type(node)}")
    return node


def tfm2(env: Env, nn: NodeName) -> Tuple[InCont, OutCont]:
    node = env[nn]
    if not isTfm2(node):
        raise RuntimeError(f"{nn} is supposed to be tfm2 not {type(node)}")
    return node


def tfm1Conts(env: Env) -> PVector[Tuple[InCont, OutCont]]:
    return pvector(map(lambda x: tfm1(env, x), tfms1(env)))


def tfm2Conts(env: Env) -> PVector[Tuple[InCont, OutCont]]:
    return pvector(map(lambda x: tfm2(env, x), tfms2(env)))
