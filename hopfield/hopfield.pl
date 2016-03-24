:- module(hopfield).
:- use_module("../lib/list").
:- use_module("../lib/vector").
:- use_module("../lib/matrix").

:- export hopfield/2.

% generate_lists(+L, +E, -Ls).
% Given natural number C, list L and element E, Ls is the list of lists that
% occur by inserting element E at all different positions in L.
generate_lists(L, E, Ls) :-
    findall(NL, insert(E, L, NL), Ls).

% generate_identity_matrix(+D, +E, -M).
% Given dimension M and element E, M is an DxD matrix identity matrix of E.
generate_identity_matrix(0, _, []).
generate_identity_matrix(D, E, M) :-
    ND is D - 1,
    generate_list(ND, L),
    generate_lists(L, E, M).

% vector_length(+Vs, -L).
% Given a list of vectors Vs of the same length, L is their length.
vector_length([V|_], L) :-
    length(V, L).

% hopfield(+Vs, -W).
% Given a list of vectors Vs, W is the weight matrix of a hopfield network
% whose states are the Vs vectors.
hopfield(Vs, W) :-
    length(Vs, VecCount),
    vector_length(Vs, VecLength),
    generate_identity_matrix(VecLength, VecCount, IdentityMatrix),
    multi_self_vv_mult(Vs, Ms),
    multi_mm_add(Ms, M),
    mm_sub(M, IdentityMatrix, W).

