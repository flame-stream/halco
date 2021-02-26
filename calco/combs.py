from typing import Callable
import apache_beam as beam


def reduceNode(k, reducer) -> Callable[[beam.PCollection], beam.PCollection]:
    return lambda pcoll: (pcoll
                          | beam.GroupBy(k)
                          | beam.CombineValues(reducer)
                          | beam.Map(lambda e: e[1]))


def coReduceNode(
    k1
)
