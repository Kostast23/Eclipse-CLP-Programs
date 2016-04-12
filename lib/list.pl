:- module(list).

:- export generate_list/3.
:- export halve/3.
:- export insert/3.
:- export map_list_to_position/2.
:- export multi_append/2.
:- export remove/3.
:- export sublist/2.


% generate_list(+C, +E, -L).
% Given natural number C and element E, L is a list of C count Es.
generate_list(0, _, []).
generate_list(C, E, [E|L]) :-
    C > 0,
    C1 is C - 1,
    generate_list(C1, E, L).


% halve(+L, -L1, -L2).
% Given list L, L1 and L2 are the two halfs of L.
% If the length of L is 2*n+1, L1 is of length n and L2 is of length n + 1.
halve(L, L1, L2) :-
    halve(L, L, L1, L2).

% halve(+L, +L, -L1, -L2).
% The first and the second argument have to be the list to be halved.
% Iteratively remove 2 elements from the first and 1 element from the second.
% When the first argument list ends up being [] or [_], the second argument list
% is the second half of the original list.
halve([], L2, [], L2).
halve([_], L2, [], L2).
halve([_,_|T], [X|L], [X|L1], L2) :-
    halve(T, L, L1, L2).


% insert(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of inserting E anywhere in L1.
insert(E, L1, L2) :-
    remove(E, L2, L1).


% map_list_to_position(+L, -ML).
% Given a list L maps every element of L to its position in the list by
% creating a list of tuples of the form (element, position).
map_list_to_position(L, ML) :-
    map_list_to_position(L, 1, ML).
map_list_to_position([], _, []).
map_list_to_position([H|T], Pos, [(H, Pos)|MT]) :-
    NextPos is Pos + 1,
    map_list_to_position(T, NextPos, MT).


% multi_append(+Lists, ?FinalList).
% FinalList is obtained by concatenating all the Lists.
multi_append([], []).
multi_append([L|Ls], FinalList) :-
    multi_append(Ls, TempList),
    append(L, TempList, FinalList).


% remove(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of remove E from anywhere in L1.
remove(E, [E|L], L).
remove(E, [X|L1], [X|L2]) :-
    remove(E, L1, L2).


% sublist(+L1, +L2)
% Checks if list L1 is a sublist of L2.
% Avoid using this predicate to find all the possible sublists of L2 as the
% empty list may appear multiple times in the result.
sublist(L1, L2) :-
    append(L3, _, L2),
    append(_, L1, L3).

