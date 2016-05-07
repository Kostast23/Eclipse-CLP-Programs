In graph theory, graph coloring is an assignment of labels traditionally called
"colors" to elements of a graphsubject to certain constraints. In its simplest
form, it is a way of coloring the vertices of a graph such that no two adjacent
vertices share the same color; this is called a vertex coloring. The smallest
number of colors needed to color a graph G is called its chromatic number.
[source: Wikipedia](https://en.wikipedia.org/wiki/Graph_coloring)

Using module *graph* we can create a graph with N vertices and density D.
The *graph* module is work of *P. Stamatopoulos*.

We attempt to solve both the vertex coloring and the chromatic number problem
of a given graph using constraint logic programming and the ECLIPSe libraries
[fd](http://eclipseclp.org/doc/bips/lib/fd/index.html) and [ic](http://eclipseclp.org/doc/bips/lib/ic/index.html).
