:- module(set).

:- export set/2.
:- export set/3.
:- export var_set/2.
:- export var_set/3.


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


% var_member(+Var, +List)
% True if variable Var is a member of List).
var_member(V1, [V2|_]) :-
    V1 == V2,
    !.
var_member(V, [_|Vs]) :-
    var_member(V, Vs).

% var_set(+List, ?Set).
% var_set/2 extends set/2 by taking into account variables.
var_set(List, Set) :-
    var_set(List, [], Set).

% set(+List, +Seen, ?Set).
% var_set/3 extends set/3 by taking into account variables.
var_set([], _, []).
var_set([H|T], Seen, [H|Set]) :-
    not var_member(H, Seen),
    var_set(T, [H|Seen], Set).
var_set([H|T], Seen, Set) :-
    var_member(H, Seen),
    var_set(T, Seen, Set).

