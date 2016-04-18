:- use_module("../lib/list").


% langford(+N, -L).
% Given number N > 1, L is the corresponding Langford sequence.
% If there are more than one sequences they can be found through recursion.
% For every sequence there is a symmetric sequence that will be found.
langford(N, L) :-
    N > 0,
    Len is 2 * N,
    length(L, Len),
    langford_subsequences(N, L).
    
langford_subsequence(N, S) :-
    length(Mid, N),
    append([N|Mid], [N], S).

langford_subsequences(0, _).
langford_subsequences(N, L) :-
    N > 0,
    langford_subsequence(N, S),
    sublist(S, L),
    NN is N - 1,
    langford_subsequences(NN, L).

% langford_3_9(-L).
% Recursively finds all the L langford sequences of 3n numbers where n is 9.
% Implemented as a challenge of not using any arithmetic predicates.
% Unify L with a list of 27 units. Check for the numbers from 1 to 9 that lists
% where each pair of number k is k units apart are sublists of L.
% Start from the sublist of 9 and continue in reverse order to minimize the
% possible configurations.
langford_3_9(L) :-
    L = [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
    sublist([9,_,_,_,_,_,_,_,_,_,9,_,_,_,_,_,_,_,_,_,9], L),
    sublist([8,_,_,_,_,_,_,_,_,8,_,_,_,_,_,_,_,_,8], L),
    sublist([7,_,_,_,_,_,_,_,7,_,_,_,_,_,_,_,7], L),
    sublist([6,_,_,_,_,_,_,6,_,_,_,_,_,_,6], L),
    sublist([5,_,_,_,_,_,5,_,_,_,_,_,5], L),
    sublist([4,_,_,_,_,4,_,_,_,_,4], L),
    sublist([3,_,_,_,3,_,_,_,3], L),
    sublist([2,_,_,2,_,_,2], L),
    sublist([1,_,1,_,1], L).

