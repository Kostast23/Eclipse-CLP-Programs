% To run the tests open the command line eclipse environment and then execute
% lib(test_util).           to load the test_util library
% test(tests).              to execute the tests

use_module(games_csp).


games_csp([4,1,2,3],5,2,Gs,P) should_give Gs == [5,1,1,4], P == 35.
games_csp([4,-1,-2,3],5,2,Gs,P) should_give Gs == [5,1,1,4], P == 29.
games_csp([4,1,-2,3,4],3,2,Gs,P) should_give Gs == [3,2,1,2,3], P == 30.
games_csp([2,-3,4,1,-2,2,1],8,3,Gs,P) should_give Gs == [5,1,8,1,1,7,3], P == 55.
games_csp([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],1,1,Gs,P) should_give Gs == [1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], P == -35.
games_csp([5,3,-1,2,4,6,-7,5,6,7,8,2,-3,4],16,12,Gs,P) should_give Gs == [16,12,
     1,12,12,16,1,12,12,12,16,12,1,16], P == 705.
games_csp([1,2,3,4,5,6,5,4,3,2,1],10,8,Gs,P) should_give Gs == [8,8,8,8,8,10,8,
    8,8,8,8], P == 300.

games_csp([3,-2,4,-5,2,0,4,-1,3,4],5,2,Gs,P) should_give multiple_solutions(C, C == 2,
    ( Gs == [3,1,5,1,1,1,4,1,1,5], P == 62
    ; Gs == [3,1,5,1,1,1,5,1,1,4], P == 62
    )).
games_csp([1,2,3,4,5,0,-1,-2,-3,-4,-5],5,4,Gs,P) should_give multiple_solutions(C, C == 4,
    ( Gs == [4,4,4,4,5,1,1,1,1,1,1], P == 50
    ; Gs == [4,4,4,4,5,2,1,1,1,1,1], P == 50
    ; Gs == [4,4,4,4,5,3,1,1,1,1,1], P == 50
    ; Gs == [4,4,4,4,5,4,1,1,1,1,1], P == 50
    )).
games_csp([2,4,1,-3,3,-4,2,3,5,4,5],6,2,Gs,P) should_give multiple_solutions(C, C == 8,
    ( Gs == [2,6,1,1,3,1,1,1,6,1,3], P == 85
    ; Gs == [2,6,1,1,3,1,1,1,5,1,4], P == 85
    ; Gs == [2,6,1,1,3,1,1,1,4,1,5], P == 85
    ; Gs == [2,6,1,1,3,1,1,1,3,1,6], P == 85
    ; Gs == [2,6,1,1,2,1,1,2,6,1,3], P == 85
    ; Gs == [2,6,1,1,2,1,1,2,5,1,4], P == 85
    ; Gs == [2,6,1,1,2,1,1,2,4,1,5], P == 85
    ; Gs == [2,6,1,1,2,1,1,2,3,1,6], P == 85
    )).
games_csp([0],3,1,Gs,P) should_give multiple_solutions(C, C == 3,
    ( Gs == [3], P == 0
    ; Gs == [2], P == 0
    ; Gs == [1], P == 0
    )).
games_csp([2,7,-1,5,0,2,1,4,3],8,5,Gs,P) should_give multiple_solutions(C, C == 2,
    ( Gs == [5,8,1,8,2,8,2,8,5], P == 170
    ; Gs == [5,8,1,8,1,8,2,8,5], P == 170
    )).
findall(Gs,games_csp([1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3],5,1,Gs,_),L),
    length(L,N) should_give N == 210.

