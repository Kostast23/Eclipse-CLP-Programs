% To run the tests open the command line eclipse environment and then execute
% lib(test_util).           to load the test_util library
% test(tests).              to execute the tests

use_module(alphametic).


alphametic([[A,S],[A]],[M,O,M]) should_give A == 9, M == 1, O == 0, S == 2.
alphametic([[S,E,N,D],[M,O,R,E]],[M,O,N,E,Y]) should_give
    S == 9, E == 5, N == 6, D == 7, M == 1, O == 0, R == 8, Y == 2.
alphametic([[I,F],[T,H,O,U],[D,I,D,S,T],[T,H,I,S],[D,E,E,D],[O,F]],[D,E,A,T,H]) should_give
    A == 4, D == 2, E == 9, F == 8, H == 5, I == 3, O == 7, S == 0, T == 1, U == 6.
alphametic([[I],[W,I,L,L],[P,A,Y],[T,H,E]],[T,H,E,F,T]) should_give
    A == 2, E == 4, F == 7, H == 0, I == 8, L == 3, P == 5, T == 1, W == 9, Y == 6.
alphametic([[A,N,D],[U,N,T,O],[M,O,U,N,T]],[H,E,R,M,O,N]) should_give
    A == 6, D == 8, E == 0, H == 1, M == 9, N == 5, O == 4, R == 2, T == 3, U == 7.
alphametic([[T,H,A,T],[T,H,Y],[S,H,A,M,E],[M,A,Y]],[A,P,P,E,A,R]) should_give
    A == 1, E == 5, H == 3, M == 4, P == 0, R == 7, S == 9, T == 6, Y == 8.
alphametic([[A,N,D],[A,L,L],[H,A,N,D,S],[S,H,A,L,L],[B,E]],[F,E,E,B,L,E]) should_give
    A == 4, B == 3, D == 7, E == 2, F == 1, H == 6, L == 9, N == 8, S == 5.
alphametic([[W,H,I,C,H],[W,A,S],[W,I,T,H],[T,H,E]],[F,A,T,H,E,R]) should_give
    A == 0, C == 6, E == 3, F == 1, H == 7, I == 4, R == 2, S == 5, T == 8, W == 9.
alphametic([[C,I,R,C,L,E],[E,L,L,I,P,S,E],[E,U,L,E,R],[S,E,R,I,E,S],[S,U,M]],[C,A,L,C,U,L,U,S]) should_give
    A == 0, C == 1, E == 9, I == 8, L == 3, M == 7, P == 2, R == 5, S == 6, U == 4.
alphametic([[A,L,D,E,R],[M,A,P,L,E],[E,L,D,E,R],[P,A,L,M]],[T,R,E,E,S]) should_give
    A == 3, D == 6, E == 1, L == 4, M == 2, P == 5, R == 8, S == 9, T == 7.

