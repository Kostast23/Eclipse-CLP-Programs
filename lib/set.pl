:- module(set).

:- export set/2.
:- export set/3.

% set(+List, ?Set).
% Set is obtained by keeping only one occurence of every element in List.
set(List, Set) :-
    set(List, [], Set).

% set(+List, +Seen, ?Set).
% Set is obtained by keeping only one occurence of every element in List.
% An element is added in Seen and Set if it is not already in Seen.
set([], _, []).
set([H|T], Seen, [H|Set]) :-
    not member(H, Seen),
    set(T, [H|Seen], Set).
set([H|T], Seen, Set) :-
    member(H, Seen),
    set(T, Seen, Set).

