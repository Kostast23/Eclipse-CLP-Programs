A key process in a [Hopfield network](https://en.wikipedia.org/wiki/Hopfield_network) is training, ie storing in it a number of
N-dimensional vectors. In this process a NxN matrix of the weights of the network
is created. Each weight of the matrix corresponds to a connection between two nodes
of the network and quantifies the effect of the connection.

If we want to store on a Hopfield network M N-dimensional vectors, then the
weight matrix W of the network is calculated by the formula

![Weight matrix formula](https://github.com/Kostast23/Eclipse-CLP-Programs/blob/master/hopfield/misc/formula.gif)

where ![Yi](https://github.com/Kostast23/Eclipse-CLP-Programs/blob/master/hopfield/misc/formula_yi.gif) is one of the N-dimensional vector-rows to store, ![Yi_T](https://github.com/Kostast23/Eclipse-CLP-Programs/blob/master/hopfield/misc/formula_yit.gif) is the
N-dimensional vector-column, which is the inverse of ![Yi](https://github.com/Kostast23/Eclipse-CLP-Programs/blob/master/hopfield/misc/formula_yi.gif), and ![I_N](https://github.com/Kostast23/Eclipse-CLP-Programs/blob/master/hopfield/misc/formula_in.gif) is the NxN
identity matrix.

The hopfield/2 predicate of the hopfield module given a list of N-dimensional
vectors computes the weight matrix of the network according to the above formula.

