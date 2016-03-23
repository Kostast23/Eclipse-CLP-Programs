% remove(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of remove E from anywhere in L1.
remove(E, [E|L], L).
remove(E, [X|L1], [X|L2]) :-
    remove(E, L1, L2).

% insert(+E, +L1, -L2).
% Given element E and list L1, L2 is the result of inserting E anywhere in L1.
insert(E, L1, L2) :-
    remove(E, L2, L1).

% gen_list(+C, -L).
% Given natural number C, L is a list of C zeros.
gen_list(0, []).
gen_list(C, [0|L]) :-
    C > 0,
    C1 is C - 1,
    gen_list(C1, L).

% gen_lists(+L, +E, -Ls).
% Given natural number C, list L and element E, Ls is the list of lists that
% occur by inserting element E at all different positions in L.
gen_lists(L, E, Ls) :-
    findall(NL, insert(E, L, NL), Ls).

% gen_identity_matrix(+D, +E, -M).
% Given dimension M and element E, M is an DxD matrix identity matrix of E.
gen_identity_matrix(0, _, []).
gen_identity_matrix(D, E, M) :-
    ND is D - 1,
    gen_list(ND, L),
    gen_lists(L, E, M).

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

% many_self_vv_mult(+Vs, -Ms).
% Given a list of vectors Vs, Ms is the list of matrices of the multiplication
% of each vector with itself.
many_self_vv_mult([], []).
many_self_vv_mult([V|Vs], [M|Ms]) :-
    vv_mult(V, V, M),
    many_self_vv_mult(Vs, Ms).

% vv_sub(+V1, +V2, -RV).
% Given vectors V1 and V2, RV is the result of their subtraction.
vv_sub([], [], []).
vv_sub([E1|V1], [E2|V2], [R|RV]) :-
    R is E1 - E2,
    vv_sub(V1, V2, RV).

% mm_sub(+M1, +M2, -RM).
% Given matrices M1 and M2, RM is the result of their subtraction.
mm_sub([], [], []).
mm_sub([V1|M1], [V2|M2], [RV|RM]) :-
    vv_sub(V1, V2, RV),
    mm_sub(M1, M2, RM).

% vv_add(+V1, +V2, -RV).
% Given vectors V1 and V2, RV is the result of their addition.
vv_add([], [], []).
vv_add([E1|V1], [E2|V2], [R|RV]) :-
    R is E1 + E2,
    vv_add(V1, V2, RV).

% mm_add(+M1, +M2, -RM).
% Given matrices M1 and M2, RM is the result of their addition.
mm_add([], [], []).
mm_add([V1|M1], [V2|M2], [RV|RM]) :-
    vv_add(V1, V2, RV),
    mm_add(M1, M2, RM).

% many_mm_add(+Ms, -M).
% Given a list of matrices Ms, M is the result of their addition.
many_mm_add([M], M).
many_mm_add([M1, M2|Ms], R) :-
    mm_add(M1, M2, R1),
    many_mm_add([R1|Ms], R).

% vector_length(+Vs, -L).
% Given a list of vectors Vs of the same length, L is their length.
vector_length([V|_], L) :-
    length(V, L).

hopfield(Vs, W) :-
    length(Vs, VecCount),
    vector_length(Vs, VecLength),
    gen_identity_matrix(VecLength, VecCount, IdentityMatrix),
    many_self_vv_mult(Vs, Ms),
    many_mm_add(Ms, M),
    mm_sub(M, IdentityMatrix, W).
