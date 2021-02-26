from operator import itemgetter
import apache_beam as beam

from ..conts import Attrs, PropGroup, prop, OutCont, InCont
from ..cgraph import stream, tfm1, tfm2

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


@stream(OutCont(attrs={friend.personId, friend.petId}))
def friends(pipeline: beam.Pipeline) -> beam.PCollection:
    return pipeline | beam.Create(
        dict(zip(friend, e)) for e in [
            [1, 2],
            [4, 3]
        ]
    )


@stream(OutCont(attrs={specie.id, specie.name}))
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
def petNamesStats(p) -> beam.PCollection:
    # TODO side effects
    # TODO calcs
    return p


@tfm1(
    InCont(
        attrs={person.name, pet.name},
        props={sameAge, noFromMesozoic}
    ),
    OutCont()
)
def priceNames(p) -> beam.PCollection:
    # TODO side
    return p


@tfm1(
    InCont(
        attrs={person.name, specie.name},
        props={noFromMesozoic}
    ),
    OutCont()
)
def nameSpeciesCorrelation(p) -> beam.PCollection:
    # TODO side
    return p


@tfm1(
    InCont(attrs={pet.age}),
    OutCont(props={noFromMesozoic})
)
def filterMesozoic(p) -> beam.PCollection:
    return p | beam.Filter(lambda e: e[pet.age] < 100500)


@tfm1(
    InCont(attrs={"pet.age", "person.age"}),
    OutCont(props={sameAge})
)
def filterSameAge(p) -> beam.PCollection:
    return p | beam.Filter(lambda e: e[pet.age] == e[person.age])


@tfm2(
    InCont(props={pet.id}),
    InCont(props={friend.petId}),
    OutCont()
)
def joinPetsFriends(p1, p2) -> beam.PCollection:
    return combinators.coReduceJoin(
        itemgetter(pet.id), itemgetter(friend.petId)
    )(p1, p2)


@tfm2(
    InCont(props={person.id}),
    InCont(props={friend.personId}),
    OutCont()
)
def joinPersonsFriends(p1, p2) -> beam.PCollection:
    return combinators.coReduceJoin(
        itemgetter(person.id), itemgetter(friend.personId)
    )(p1, p2)


@tfm2(
    InCont(props={pet.speciesId}),
    InCont(props={specie.id}),
    OutCont()
)
def joinPetsSpecies(p1, p2) -> beam.PCollection:
    return combinators.coReduceJoin(
        itemgetter(pet.speciesId), itemgetter(specie.id)
    )(p1, p2)


if __name__ == '__main__':
    # Не только dir, но и импорты надо инспектить
    cgraph = CGraph(
        globals=globals(),
        semantics=[petNamesStats, priceNames, nameSpeciesCorrelation],
    )
    graphs = cgraph.gen_graphs()
    drawGraphs(take(graphs, 10))
    i = int(input())
    pipeline_options = None  # TODO
    with beam.Pipeline(options=pipeline_options) as pipeline:
        graphs[i].eval(pipeline)
