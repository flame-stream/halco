from operator import itemgetter
import apache_beam as beam

from ..conts import Attrs, PropGroup, prop, OutCont, InCont
from ..cgraph import stream, tfm1, tfm2, CGraph
from .. import combs

pet = Attrs('pet', ['id', 'name', 'age', 'speciesId'])
person = Attrs('person', ['id', 'name', 'age'])
friend = Attrs('friend', ['personId', 'petId'])
specie = Attrs('specie', ['id', 'name'])

filterPetProps = PropGroup()

sameAge = prop('sameAge', filterPetProps)
noFromMesozoic = prop('noFromMesozoic', filterPetProps)


@stream(OutCont(attrs=pet))
def pets(pipeline: beam.Pipeline) -> beam.PCollection:
    return pipeline | beam.Create(
        dict(zip(pet, e)) for e in [
            [1, 'a', 12, 3],
            [2, 'b', 1, 1],
            [3, 'bobik', 10005000, 2],
            [4, 'd', 10, 1],
        ]
    )


@stream(OutCont(attrs=person))
def persons(pipeline: beam.Pipeline) -> beam.PCollection:
    return pipeline | beam.Create(
        dict(zip(person, e)) for e in [
            [1, 'Bob', 5],
            [2, 'Sarah', 1],
            [3, 'Lee Jae Dong', 30],
            [4, 'Stork', 10005000],
            [5, 'Byun', 28],
        ]
    )


@stream(OutCont(attrs=friend))
def friends(pipeline: beam.Pipeline) -> beam.PCollection:
    return pipeline | beam.Create(
        dict(zip(friend, e)) for e in [
            [1, 2],
            [4, 3]
        ]
    )


@stream(OutCont(attrs=specie))
def species(pipeline: beam.Pipeline) -> beam.PCollection:
    return pipeline | beam.Create(
        dict(zip(specie, e)) for e in [
            [3, 'dog'],
            [1, 'cat'],
            [2, 'Tyrannosaurus']
        ]
    )


@tfm1(
    InCont(
        attrs={pet.name},
        noprops=filterPetProps
    ),
    OutCont()
)
def pet_names_stats(pcoll: beam.PCollection) -> beam.PCollection:
    # TODO side effects
    # TODO calcs
    return pcoll


@tfm1(
    InCont(
        attrs={person.name, pet.name},
        props={sameAge, noFromMesozoic}
    ),
    OutCont()
)
def price_names(pcoll: beam.PCollection) -> beam.PCollection:
    # TODO side
    return pcoll


@tfm1(
    InCont(
        attrs={person.name, specie.name},
        props={noFromMesozoic}
    ),
    OutCont()
)
def name_species_correlation(pcoll: beam.PCollection) -> beam.PCollection:
    # TODO side
    return pcoll


@tfm1(
    InCont(attrs={pet.age}),
    OutCont(props={noFromMesozoic})
)
def filter_mesozoic(pcoll: beam.PCollection) -> beam.PCollection:
    return pcoll | beam.Filter(lambda e: e[pet.age] < 100500)


@tfm1(
    InCont(attrs={"pet.age", "person.age"}),
    OutCont(props={sameAge})
)
def filter_same_age(pcoll: beam.PCollection) -> beam.PCollection:
    return pcoll | beam.Filter(lambda e: e[pet.age] == e[person.age])


@tfm2(
    InCont(props={pet.id}),
    InCont(props={friend.petId}),
    OutCont()
)
def join_pets_friends(
    pcoll1: beam.PCollection,
    pcoll2: beam.PCollection
) -> beam.PCollection:
    return combs.inner_join_node(
        itemgetter(pet.id), itemgetter(friend.petId)
    )(pcoll1, pcoll2)


@tfm2(
    InCont(props={person.id}),
    InCont(props={friend.personId}),
    OutCont()
)
def join_persons_friends(
    pcoll1: beam.PCollection,
    pcoll2: beam.PCollection
) -> beam.PCollection:
    return combs.inner_join_node(
        itemgetter(person.id), itemgetter(friend.personId)
    )(pcoll1, pcoll2)


@tfm2(
    InCont(props={pet.speciesId}),
    InCont(props={specie.id}),
    OutCont()
)
def join_pets_species(
    pcoll1: beam.PCollection,
    pcoll2: beam.PCollection
) -> beam.PCollection:
    return combs.inner_join_node(
        itemgetter(pet.speciesId), itemgetter(specie.id)
    )(pcoll1, pcoll2)


if __name__ == '__main__':
    cgraph = CGraph(
        globals=globals(),
        semantics=[pet_names_stats, price_names, name_species_correlation],
    )
    graphs = cgraph.gen_graphs()
    # TODO drawGraphs(take(graphs, 10))
    i = int(input())
    pipeline_options = None  # TODO
    with beam.Pipeline(options=pipeline_options) as pipeline:
        graphs[i].eval(pipeline)
