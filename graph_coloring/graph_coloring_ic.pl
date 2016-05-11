:- module(graph_coloring_ic).

:- export graph_coloring/4.

:- lib(ic).
:- lib(branch_and_bound).
:- use_module(graph).


% graph_coloring(+Nodes, +Density, ?NodesColor, ?Chromatic).
% Given a number of nodes and a number for the density of the graph create a
% random graph and find a graph coloring with the least colors used.
% NodesColor is a list with the color of every node and Chromatic is the chromatic
% number of the graph.
graph_coloring(Nodes, Density, NodesColor, Chromatic) :-
    create_graph(Nodes, Density, Graph),
    length(NodesColor, Nodes),
    NodesColor #:: 1..Nodes,
    constrain(Graph, NodesColor),
    ic:max(NodesColor, Chromatic),
    bb_min(search(NodesColor, 0, first_fail, indomain, complete, []), Chromatic,
        bb_options{strategy:restart}).

% kth1(+L, +K, ?X).
% X is the element at index K of list L.
kth1([X|_], 1, X).
kth1([_|L], K, X) :-
    K > 1,
    NK is K - 1,
    kth1(L, NK, X).

% constrain(+Graph, +NodesColor).
% Constrain connected nodes of Graph to have different colors.
constrain([], _).
constrain([X - Y|Graph], NodesColor) :-
    kth1(NodesColor, X, NodeXColor),
    kth1(NodesColor, Y, NodeYColor),
    NodeXColor #\= NodeYColor,
    constrain(Graph, NodesColor).

