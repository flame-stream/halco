from typing import Callable, Iterable, Dict, Tuple, TypeVar, List, NewType
import apache_beam as beam


_StreamName = NewType('_StreamName', str)

_E = TypeVar('_E')
_K = TypeVar('_K')


Tfm1 = NewType('Tfm1', Callable[[beam.PCollection], beam.PCollection])
Tfm2 = NewType('Tfm2', Callable[[beam.PCollection, beam.PCollection], beam.PCollection])


def reduce_node(
    k: Callable[[E], _K],
    reducer: Callable[[List[E]], Iterable[E]]
) -> Tfm1:
    return lambda pcoll: (pcoll
                          | beam.GroupBy(k)
                          | beam.CombineValues(reducer)
                          | beam.FlatMap(lambda e: e[1]))


def co_reduce_node(
    name1: _StreamName,
    name2: _StreamName,
    k1: Callable[[_E], _K],
    k2: Callable[[_E], _K],
    reducer: Callable[
        [_K, List[_E], List[_E]],
        Iterable[_E]
    ]
) -> Tfm2:

    def apply_reducer(co_group: Tuple[_K, Dict[_StreamName, _E]]) -> Iterable[_E]:
        k, d = co_group
        return reducer(k, d[name1], d[name2])

    def inner(pcoll1: beam.PCollection, pcoll2: beam.PCollection) -> beam.PCollection:
        pcoll1 = pcoll1 | beam.Map(lambda e: (k1(e), e))
        pcoll2 = pcoll2 | beam.Map(lambda e: (k2(e), e))
        return ({name1: pcoll1, name2: pcoll2}
                | beam.CoGroupByKey()
                | beam.FlatMap(apply_reducer))

    return inner


def inner_join_node(
    k1: Callable[[_E], _K],
    k2: Callable[[_E], _K],
) -> Tfm2:
    # TODO maybe same elements in reducer.
    return co_reduce_node(
        name1='left', name2='right',
        k1=k1, k2=k2,
        reducer=lambda _, es1, es2: (e1 | e2 for e1 in es1 for e2 in es2)
    )
