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
