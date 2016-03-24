:- module(matrix).
:- use_module("../vector/vector.pl").

:- export mm_add/3.
:- export multi_mm_add/2.
:- export mm_sub/3.

% mm_add(+M1, +M2, -RM).
% Given matrices M1 and M2, RM is the result of their addition.
mm_add([], [], []).
mm_add([V1|M1], [V2|M2], [RV|RM]) :-
    vv_add(V1, V2, RV),
    mm_add(M1, M2, RM).

% multi_mm_add(+Ms, -M).
% Given a list of matrices Ms, M is the result of their addition.
multi_mm_add([M], M).
multi_mm_add([M1, M2|Ms], R) :-
    mm_add(M1, M2, R1),
    multi_mm_add([R1|Ms], R).

% mm_sub(+M1, +M2, -RM).
% Given matrices M1 and M2, RM is the result of their subtraction.
mm_sub([], [], []).
mm_sub([V1|M1], [V2|M2], [RV|RM]) :-
    vv_sub(V1, V2, RV),
    mm_sub(M1, M2, RM).

