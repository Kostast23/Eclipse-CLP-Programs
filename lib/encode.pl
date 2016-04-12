:- module(encode).

:- export run_length_decoding/2.


% run_length_decoding(+Enc, -Dec).
% Given an encoded run-length list of (run-length, element) tuples, produces
% the decoded list.
run_length_decoding([], []).
run_length_decoding([(C, E)|T], L) :-
    generate_list(C, E, L1),
    run_length_decoding(T, L2),
    append(L1, L2, L).

