Output format of YATES
----------------------
For each topology, YATES will create a directory
`$YATES_ROOT/data/results/<topology>/` where the results will be stored (unless
another output directory is specified with `-out` flag). The ouput directory
usually consists of the following files and sub-directories. The input contains
a timeseries of traffic matrices where each traffic matrix corresponds to a snapshot or
aggregated (over a time window) measurement of demands between nodes in the topology.
YATES iterates over the list of traffic matrices in order and for each iteration (point
in the timeseries), it generates performance statistics. As the output consists of 
statistics for each traffic matrix, the final output is also a a timeseries,
corresponding to the input.

| File                                  |  Data description                                                                               |
| --------------------------------------|-------------------------------------------------------------------------------------------------|
| `CongestionLossVsIterations.dat`      | Timeseries of traffic dropped due to insufficient link capacity. When a link receives more traffic than its capacity, the excess traffic is recorded as dropped. The value reported here is the fraction of total demand that was dropped in this way.                               |
| `EdgeCongestionVsIterations.dat`      | Timeseries of link utilization for each edge. Link utilization is the ratio of traffic carried by a link to the capacity of that link. This value is always <= 1.                                                   |
| `EdgeExpCongestionVsIterations.dat`   | Timeseries of scheduled (assuming no traffic drops) link load for each edge. Scheduled link load is the ratio of total traffic that was scheduled to be carried by a link to the capacity of the link. This value can be greater than 1 if the TE scheduled traffic on a link more than the link's capacity                   |
| `FailureLossVsIterations.dat`         | Timeseries of traffic dropped due to failures. If traffic couldn't be routed because of failures in the topology, it is marked as dropped. The value reported here is the fraction of total demand that was dropped in this way.                                                   |
| `k10CongestionVsIterations.dat`       | Timeseries of 10-th percentile link utilization. See definition of link utilization above. Each link has a utilization value for each iteration. For each iteration, percentiles are computed over the link utilizations for all links.                                                 |
| `k10ExpCongestionVsIterations.dat`    | Timeseries of 10-th percentile scheduled link load. See defintion of scheduled link load above. Similar to the above computation with scheduled link loads.                                              |
| `k20CongestionVsIterations.dat`       | Timeseries of 20-th percentile link utilization.                                                |
| `k20ExpCongestionVsIterations.dat`    | Timeseries of 20-th percentile scheduled link load.                                             |
| `k30CongestionVsIterations.dat`       | Timeseries of 30-th percentile link utilization.                                                |
| `k30ExpCongestionVsIterations.dat`    | Timeseries of 30-th percentile scheduled link load.                                             |
| `k40CongestionVsIterations.dat`       | Timeseries of 40-th percentile link utilization.                                                |
| `k40ExpCongestionVsIterations.dat`    | Timeseries of 40-th percentile scheduled link load.                                             |
| `k50CongestionVsIterations.dat`       | Timeseries of 50-th percentile link utilization.                                                |
| `k50ExpCongestionVsIterations.dat`    | Timeseries of 50-th percentile scheduled link load.                                             |
| `k60CongestionVsIterations.dat`       | Timeseries of 60-th percentile link utilization.                                                |
| `k60ExpCongestionVsIterations.dat`    | Timeseries of 60-th percentile scheduled link load.                                             |
| `k70CongestionVsIterations.dat`       | Timeseries of 70-th percentile link utilization.                                                |
| `k70ExpCongestionVsIterations.dat`    | Timeseries of 70-th percentile scheduled link load.                                             |
| `k80CongestionVsIterations.dat`       | Timeseries of 80-th percentile link utilization.                                                |
| `k80ExpCongestionVsIterations.dat`    | Timeseries of 80-th percentile scheduled link load.                                             |
| `k90CongestionVsIterations.dat`       | Timeseries of 90-th percentile link utilization.                                                |
| `k90ExpCongestionVsIterations.dat`    | Timeseries of 90-th percentile scheduled link load.                                             |
| `k95CongestionVsIterations.dat`       | Timeseries of 95-th percentile link utilization.                                                |
| `k95ExpCongestionVsIterations.dat`    | Timeseries of 95-th percentile scheduled link load.                                             |
| `LatencyDistributionVsIterations.dat` | Timeseries of distribution of latency. For each iteration, this distribution shows what fraction of the total demand experienced how much latency. Latency could be in units of time, or number of hops as specified in the input.                                                           |
| `MaxCongestionVsIterations.dat`       | Timeseries of max. link utilization. For each iteration, report the utilization of the link with the maximum value of link utilization. See definition of link utilization above.                                                             |
| `MaxExpCongestionVsIterations.dat`    | Timeseries of max. scheduled link load. For each iteration, report the schediuled link load of the link with the maximum value of expected link load. See definition of scheduled link load above.                                                         |
| `MeanCongestionVsIterations.dat`      | Timeseries of mean link utilization. (Similar to the max. congestion above, but report mean over all links)                                                            |
| `MeanExpCongestionVsIterations.dat`   | Timeseries of mean scheduled link load. (Similar to the max. scheduled link load above, but report mean over all links)                                                        |
| `NumPathsVsIterations.dat`            | Timeseries of number of paths used. For each iteration, report the toal number of paths used by a TE system to route traffic over.                                                             |
| `paths`                               | Output directory logging the paths used by each TE system for each iteration.                                             |
| `RecoveryChurnVsIterations.dat`       | Timeseries of path churn due to failure recovery. Recovery path churn is the number of paths that were added or removed by a TE system to handle failures.                                               |
| `TimeVsIterations.dat`                | Timeseries of time taken to compute the routing scheme for each TM.                             |
| `TMChurnVsIterations.dat`             | Timeseries of path churn due to recomputing routing scheme for each TM. This value is the number of paths that were added or removed by a TE system when updating the routing scheme in response to changing demands in subsequent iterations.                         |
| `TotalSinkThroughputVsIterations.dat` | Timeseries of total throughput to the sink node in case of flash crowds.                        |
| `TotalThroughputVsIterations.dat`     | Timeseries of normalized throughput (total traffic delivered / total demand for all node pairs).|


Note that each file will have data for all TE systems invoked during
simulation. The data will be concatenated and tagged with the name of the TE
system used. Usually, each file is in TSV format, and the header in each file
will describe the contents in each column.

**Extending YATES with more metrics**: To add more metrics of interest, you can extend the [network state](https://github.com/cornell-netlab/yates/blob/master/simulator/Simulation_Types.ml) that is maintained during simulation and update this metric appropriately during [simulation](https://github.com/cornell-netlab/yates/blob/29f2e261ca6b52d7c41c2fdd0c81ed5302d33718/simulator/Simulate_TM.ml#L682). Finally, you can log the values for the metric to a file at the end of simulation in a [similar way](https://github.com/cornell-netlab/yates/blob/c768e83da8671d8dfb9308fca4eb1188bd010f31/simulator/Yates_simulator.ml#L443) as [existing metrics](https://github.com/cornell-netlab/yates/blob/29f2e261ca6b52d7c41c2fdd0c81ed5302d33718/simulator/Simulation_Types.ml#L20).
