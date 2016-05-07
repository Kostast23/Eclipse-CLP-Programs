% To run the tests open the command line eclipse environment and then execute
% lib(test_util).           to load the test_util library
% test(tests).              to execute the tests

% These tests involve the use of random/1 and seed/1.
% It must be noted that the machine used for the development of these tests
% runs on Ubuntu 15.10 x86_64. These tests may fail under different operating
% systems due to different random executions.

use_module(graph_coloring_fd).


seed(1),     graph_coloring(10, 80, _, C) should_give C == 6.
seed(2016),  graph_coloring(15, 60, _, C) should_give C == 6.
seed(12345), graph_coloring(20, 70, _, C) should_give C == 8.
seed(1000),  graph_coloring(25, 50, _, C) should_give C == 7.
seed(9876),  graph_coloring(50, 20, _, C) should_give C == 5.
seed(10),    graph_coloring(40, 34, _, C) should_give C == 6.
seed(1111),  graph_coloring(60, 30, _, C) should_give C == 7.

