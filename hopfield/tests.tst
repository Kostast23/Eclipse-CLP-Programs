% To run the tests open the command line eclipse environment and then execute
% lib(test_util).           to load the test_util library
% test(tests).              to execute the tests

use_module(hopfield).


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

