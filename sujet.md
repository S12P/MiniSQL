# Projet de M1 DBDM

## 1 CSV MiniSQL Engine

This part is due for 2018-03-14 by mail to Emmanuel Coquery.

### 1.1 Relational Algebra Engine Over CSV

First, design a data structures to represent:

Simple relational algebra operators and expressions (selection, project, cartesian product, relation (i.e. table), renaming, minus, union). You can use a simple language for selection conditions (e.g. boolean connectors and = and < comparisons, without computation of new values).
Data to be processed (tuples, collections of tuples). You can also assume that all atomic data is of the same type (only strings or only numbers). Collections of tuples should be writable to CSV format (with a first header line)
Then, implement a simple evaluator for your relational algebra expressions:

a relation is read from a csv file named after the relation (e.g. R.csv for a relation R)
cartesian product will use nested loops
all operators will produce data in an eager way
Note that there are libraries in all proposed languages to read CSV data.

### 1.2 MiniSQL to Relational Algebra Compiler

Write an MiniSQL to algebra compiler:

MiniSQL grammar is given in the annexes
You can start with a simple version where there is no nested queries.
The IN condition can be translated into a join by pushing the FROM part of the nested query to the FROM part of the englobing query and by:

merging the WHERE condition of the nested and the global query
adding equalities between attributes of the SELECT of the nested query and attributes on the left part of the IN keyword
Note that these operations may require a renaming of the relations of the subquery

If the WHERE condition is a conjunction of atomic conditions, the NOT IN condition can be transformed into a MINUS statement between the original (sub)query without the NOT IN condition and a transformation of the original (sub)query where the NOT IN condition is replaced by an IN condition.

If the original WHERE condition is more complex it can be transformed in Disjunctive Normal Form an then transformed into =UNION=s before applying the aforementioned transformation.

## 2 A Better Algebra Engine

### 2.1 Collapsing algebra operations

Implement 2 new operators that regroup existing ones:

the ReadSelectProjectRename operator that reads a CSV file, filter lines and transform remaning tuples keeping only the proper attributes after renaming
the JoinProjectRename operator that combine two relations in a θ-join and transform resulting tuples keeping only the proper attributes after renaming (the θ-join being a cartesian product followed by a selection)
Try to use algorithms that are more efficient (in cpu and/or memory) than what would habe been done using the simple relational algebra evaluator.

### 2.2 Simple algebra expression optimization

First, write an optimizer that uses relational algebra equivalence rules to:

push down selections
add projection to discard unneeded attributes as soon as possible
replace subexpressions using the two new introduced operators
Next, try to reorder cartesian products to minimize the number of cartesian products that are not immediately restricted by a selection (i.e. that don't correspond to θ-joins).

## 3 Extensions

You can implement some of the following extensions.

### 3.1 ORDER BY

Add an ORDER BY statement to MiniSQL and implement it.

### 3.2 GROUP BY + Aggregation Functions

Add a GROUP BY statement to MiniSQL as well as a some aggregation functions (i.e. MIN, MAX, …) to MiniSQL. Devise a relational algebra operator that correspond to a SELECT ... FROM Single_Relation GROUP BY ... query and implement it. Add the GROUP BY functionality to your engine. You can make some restrictions on the positions where the GROUP BY statement occurs.

You earn 2 additional points for managing subqueries with GROUP BY that are related to the englobing query.

### 3.3 Advanced expressions on atomic values

Manage two atomic data types in your MiniSQL: strings and numbers. Add some builtin functions as well as constants to represent and manipulate these values. Modify the renaming operator so it can also create new attributes computed using atomic value expressions.

### 3.4 Other ideas

Better query optimizer using a cost model
Factorizing joins over union/minus
Various join implementations
Computing meta-data on intermediate results for dynamically choose an appropriate join implementation
Lazy evaluation (e.g. in the form of iterators) to reduce memory usage
The possibility to use temporary files if an intermediate result needs to be materialized and doesn't fit in memory (either available memory or any limit you decide)
Benchmarking various version of your query evaluator
Any idea you have
