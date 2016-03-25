:- module(list).

:- export generate_list/2.
:- export remove/3.
:- export insert/3.
:- export sublist/2.

% generate_list(+C, -L).
% Given natural number C, L is a list of C zeros.
generate_list(0, []).
generate_list(C, [0|L]) :-
    C > 0,
    C1 is C - 1,
    generate_list(C1, L).

% remove(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of remove E from anywhere in L1.
remove(E, [E|L], L).
remove(E, [X|L1], [X|L2]) :-
    remove(E, L1, L2).

% insert(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of inserting E anywhere in L1.
insert(E, L1, L2) :-
    remove(E, L2, L1).

% sublist(+L1, +L2)
% Checks if list L1 is a sublist of L2.
% Avoid using this predicate to find all the possible sublists of L2 as the
% empty list may appear multiple times in the result.
sublist(L1, L2) :-
    append(L3, _, L2),
    append(_, L1, L3).

