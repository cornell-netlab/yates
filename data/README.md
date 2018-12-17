Inputs
======
Topologies
----------
- Each topology is specified as a [`dot` file](https://en.wikipedia.org/wiki/DOT_(graph_description_language)). 
- Each link capacity should have proper units.
	
Demands (Traffic Matrices)
--------------------------
- YATES takes as input two time series of traffic matrices:
    - One corresponding the real demands that we want to consider the performance under. These TMs are used to generate the network load.
    - The other corresponding to an operators estimate of the demands during the period. These TMs are used by the TE systems to compute routing schemes.
- These traffic matrices may or may not be the same depending on the type of evaluation. For instance, to compute the robustness of TE systems to misprediction in demands, we can use the predict TMs to be different from the actual TMs.
- Format:
  - Each TM is an `n x n` matrix where `n` is the number of nodes.
  - Each row and column in this matrix is mapped to the nodes specifying the mapping in separate `<topology>.hosts` file. Line `i` in this file corresponds to the `i`-th row as well as the `i`-th column.
  - The `n x n` matrix is converted into a single line (string) by concatenating all `n` rows. This gives a single line with `n^2` numbers.
  - Each entry is considered to be specified in `bps`
  - For `T` traffic matrices, there will the `T` `n x n` matrices, which translates to `T` lines in the input file.

Notes:
- Link capacities and demands are converted to Mbps during MCF computation as `bps` cannot be used with gurobi (known issue with large numbers).

Optional data
-------------
- To use link RTTs for link weights, they can be specified in a separate file and specified using the `-rtt-file` flag. Each line in this file specifies a link and the corresponding RTT as `(<source>, <destination>)  <rtt_ms>`.
- The same format can be reused to specify any other link weights.


Outputs
=======
See [output README](results/README.md) for details.
