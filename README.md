# halco

Calco prototype written in Haskell.

Distributed dataflows can be specified in two ways: by defining an execution graph or specifying the desired result, e.g., using SQL. An execution graph consists of arbitrary operations, so it cannot be automatically transformed for optimization.
SQL is popular for data analytics, and its optimization is well-researched.
However, it is sometimes inconvenient or impossible to use SQL for general data management tasks, such as ETL, machine learning pipelines, etc.
In this work, we introduce a novel approach to specify distributed dataflows.
Our method is based on declarative specifications of user-defined operations called contracts.
Such specifications allow us to build a set of equivalent graphs, which form a space for optimization.
We implement a graphs generation prototype and outline the challenges regarding the optimization problem.
