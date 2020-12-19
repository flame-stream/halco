cograph, node = calco.cgraph(
    semantics={"write_word_count"}
)


@node
def lines() -> OutCont({"line"}):
    throw NotImplementedError()


@node
def split(...) -> ...:
    throw NotImplementedError()


@node
def count_words(...) -> ...
    throw NotImplementedError()


@node
def write_word_count(stats: InCont(attrs={"word", "count"})):
    throw NotImplementedError()
