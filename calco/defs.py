from typing import NewType, Callable, Union, Dict, Any
from enum import Enum
import apache_beam as beam


class NodeType(Enum):
    STREAM = 1
    TFM1 = 2
    TFM2 = 3


Stream = NewType('Stream', Callable[[beam.Pipeline], beam.PCollection])
Tfm1 = NewType('Tfm1', Callable[[beam.PCollection], beam.PCollection])
Tfm2 = NewType('Tfm2', Callable[[beam.PCollection, beam.PCollection], beam.PCollection])

Node = NewType('Node', Union[Stream, Tfm1, Tfm2])

Attr = NewType('Attr', str)
Prop = NewType('Prop', str)
Elem = NewType('Elem', Dict[Attr, Any])
