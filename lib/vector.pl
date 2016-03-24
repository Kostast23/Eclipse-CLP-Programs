:- module(vector).

:- export vv_add/3.
:- export vv_sub/3.
:- export cv_mult/3.
:- export vv_mult/3.
:- export multi_self_vv_mult/2.

% vv_add(+V1, +V2, -RV).
% Given vectors V1 and V2, RV is the result of their addition.
vv_add([], [], []).
vv_add([E1|V1], [E2|V2], [R|RV]) :-
    R is E1 + E2,
    vv_add(V1, V2, RV).

% vv_sub(+V1, +V2, -RV).
% Given vectors V1 and V2, RV is the result of their subtraction.
vv_sub([], [], []).
vv_sub([E1|V1], [E2|V2], [R|RV]) :-
    R is E1 - E2,
    vv_sub(V1, V2, RV).

% cv_mult(+C, +V, -RV).
% Given constant C and vector V, RV is the result of their multiplication.
cv_mult(_, [], []).
cv_mult(C, [E|Es], [R|Rs]) :-
    R is C * E,
    cv_mult(C, Es, Rs).

% vv_mult(+V1, +V2, -M).
% Given vectors V1 and V2, M is the result of their multiplication.
vv_mult([], [_|_], []).
vv_mult([E|Es], M, [R|Rs]) :-
    cv_mult(E, M, R),
    vv_mult(Es, M, Rs).

% multi_self_vv_mult(+Vs, -Ms).
% Given a list of vectors Vs, Ms is the list of matrices of the multiplication
% of each vector with itself.
multi_self_vv_mult([], []).
multi_self_vv_mult([V|Vs], [M|Ms]) :-
    vv_mult(V, V, M),
    multi_self_vv_mult(Vs, Ms).

