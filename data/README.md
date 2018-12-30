Inputs
======
Topologies
----------
- Each topology is specified as a [`dot` file](https://en.wikipedia.org/wiki/DOT_(graph_description_language)). 
- Each link capacity should have proper units.
	
Demands (Traffic Matrices)
--------------------------
- YATES takes as input two timeseries of traffic matrices (TMs):
    - One corresponding the real demands that we want to consider the performance under. This timeseries of TMs is used to generate the network load.
    - The other corresponding to an operators estimate of the demands during the period. This timeseries of TMs is used by the TE systems to compute routing schemes.
- These two timeseries of TMs may or may not be the same depending on the type of evaluation. For instance, to compute the robustness of TE systems to misprediction in demands, we can use the predicted timeseries TMs to be different from the actual TMs.
- The actual and predicted timeseries of TMs are specified as two input files.
- Format of each file is described as follows:
  - A timeseries of TMs consists of a list of TMs: one TM per user-defined time-interval. For example, the user can assume that the one TM is measured every hour. So, a list of `T` TMs corresponds to `T` hours of demands.
  - Each TM is an `n x n` matrix where `n` is the number of nodes.
  - Entry `(i, j)` in this matrix is the demand in `bps` from source node corresponding to row `i` to destination node corresponding to column `j`. 
  - Each row and column in this matrix is mapped to the nodes in the topology. This mapping is specified in a separate `<topology>.hosts` file. Row `i` and column `i` in the matrix corresponds to the node at line `i` in this hosts file.
  - Serializing a timeseries of TMs and storing in a file:
  	- Each `n x n` matrix is converted into a single line (string) by concatenating all `n` rows. This gives a single line with `n^2` numbers for one TM.
	- Since we have `T` TMs, there will be `T` `n x n` matrices. And so, each file will have `T` lines.

Notes:
- Link capacities and demands are converted to Mbps during MCF computation as `bps` cannot be used with gurobi (known issue with large numbers).

Optional data
-------------
- To use link RTTs for link weights, they can be specified in a separate file and specified using the `-rtt-file` flag. Each line in this file specifies a link and the corresponding RTT as `(<source>, <destination>)  <rtt_ms>`.
- The same format can be reused to specify any other link weights.


Outputs
=======
See [output README](results/README.md) for details.
