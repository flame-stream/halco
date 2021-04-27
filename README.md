# halco

Calco prototype written in Haskell.

There are two general ways to define computations: imperative and declarative.
The first one is more transparent for programmers, while the second one is more suitable for complex optimizations.
One declarative definition can have multiple implementations, which makes it possible to choose the most optimal one.
Accordingly, distributed dataflow can be specified in two ways: defining concrete execution graph and SQL.
A concrete graph does not capture high-level information about the problem it solves, so a distributed processing system cannot permute operations for performance purposes.
Hence, graph optimization is generally a programmer concern, but real-world graphs can consist of many nodes that make it hard to optimize them manually.
Moreover, most of the necessary information for the graph cost evaluation is available only in runtime.
SQL is popular for data analytics, and its optimization is well-researched.
However, it is inconvenient or sometimes impossible to use SQL for general data management tasks, such as ETL, machine learning pipelines, etc.

In this work, we introduce a novel approach to specify distributed dataflows.
Our method is based on declarative specifications of user-defined operations that we call contracts.
Such specifications allow us to automatically generate execution graphs with the needed semantics to choose the most optimal one.
An arbitrary operation or a dataflow part can be described by contracts, so this approach combines the transparency of the imperative approach and optimization possibilities of the declarative one.
We implement a prototype and demonstrate automatic graphs generation on a real-world dataflow.
