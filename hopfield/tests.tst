/*
To run the tests open the command line eclipse environment and then execute
- lib(test_util).           to load the test_util library
- use_module(hopfield).     to load the tested hopfield module
- ["tests.tst"].            to load the tests.tst file
- test(tests).              to execute the tests
*/


hopfield([[1,1,-1], [-1,-1,-1]], W) should_give
W = [[0, 2, 0],
     [2, 0, 0],
     [0, 0, 0]].

hopfield([[1,-1,-1,1], [-1,-1,1,-1], [1,1,1,1]], W) should_give
W = [[0, 1, -1, 3],
     [1, 0, 1, 1],
     [-1, 1, 0, -1],
     [3, 1, -1, 0]].

hopfield([[1,1,1,1,-1,1,1,1,1], [-1,1,-1,-1,1,-1,-1,1,-1], [1,-1,1,1,1,1,-1,-1,1]], W) should_give
W = [[0, -1, 3, 3, -1, 3, 1, -1, 3],
     [-1, 0, -1, -1, -1, -1, 1, 3, -1],
     [3, -1, 0, 3, -1, 3, 1, -1, 3],
     [3, -1, 3, 0, -1, 3, 1, -1, 3],
     [-1, -1, -1, -1, 0, -1, -3, -1, -1],
     [3, -1, 3, 3, -1, 0, 1, -1, 3],
     [1, 1, 1, 1, -3, 1, 0, 1, 1],
     [-1, 3, -1, -1, -1, -1, 1, 0, -1],
     [3, -1, 3, 3, -1, 3, 1, -1, 0]].

