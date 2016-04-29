:- module(alphametic).
:- use_module("../lib/list").
:- use_module("../lib/set").

:- lib(ic).

:- export alphametic/2.


% alphametic(+AddentsList, +SumVars)
% AddentsList is a list consisting of lists of variables that represent the
% digits of the addents. SumVars is a list of variables that represent the
% digits of the sum. The goal is to assign a value from 0 to 9 to each variable
% so that the sum is valid and the leftmost letter is not zero in any word.
alphametic(AddentsList, SumVars) :-
    multi_append(AddentsList, AddentsVars),
    append(AddentsVars, SumVars, AllVars),
    var_set(AllVars, SetVars),
    SetVars #:: 0..9,
    alldifferent(SetVars),
    constrain(AddentsList, SumVars),
    search(SetVars, 0, first_fail, indomain, complete, []).

constrain(Addents, SumVars) :-
    first(SumVars, S),
    S #\= 0,
    no_leading_zero_addents(Addents),
    sum_constraint(Addents, SumVars).

no_leading_zero_addents([]).
no_leading_zero_addents([[D|_]|As]) :-
    D #\= 0,
    no_leading_zero_addents(As).

sum_constraint(Addents, SumVars) :-
    multiple_arithmetic(Addents, S1),
    arithmetic(SumVars, S2),
    eval(S1) #= eval(S2).

multiple_arithmetic([], 0).
multiple_arithmetic([A|As], S1+S2) :-
    arithmetic(A, S1),
    multiple_arithmetic(As, S2).

% arithmetic(+Xs, -E).
% Given a list of variables Xs, e.g. [A,B,C], E is the mathematic expression
% 100*A + 10*B + C.
arithmetic(Xs, E) :-
    arithmetic(Xs, E, _).
arithmetic([X], X, 1).
arithmetic([X|Xs], 10*Y*X+S, 10*Y) :-
    arithmetic(Xs, S, Y).
