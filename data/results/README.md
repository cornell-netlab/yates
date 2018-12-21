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
| `CongestionLossVsIterations.dat`      | Timeseries of traffic dropped due to insufficient link capacity.                                |
| `EdgeCongestionVsIterations.dat`      | Timeseries of link utilization for each edge.                                                   |
| `EdgeExpCongestionVsIterations.dat`   | Timeseries of scheduled (assuming no traffic drops) link load for each edge.                    |
| `FailureLossVsIterations.dat`         | Timeseries of traffic dropped due to failures.                                                  |
| `k10CongestionVsIterations.dat`       | Timeseries of 10-th percentile link utilization.                                                |
| `k10ExpCongestionVsIterations.dat`    | Timeseries of 10-th percentile scheduled link load.                                             |
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
| `LatencyDistributionVsIterations.dat` | Timeseries of distribution of latency.                                                          |
| `MaxCongestionVsIterations.dat`       | Timeseries of max. link utilization.                                                            |
| `MaxExpCongestionVsIterations.dat`    | Timeseries of max. scheduled link load.                                                         |
| `MeanCongestionVsIterations.dat`      | Timeseries of mean link utilization.                                                            |
| `MeanExpCongestionVsIterations.dat`   | Timeseries of mean scheduled link load.                                                         |
| `NumPathsVsIterations.dat`            | Timeseries of number of paths used.                                                             |
| `paths`                               | Directory logging the paths used by each TE system.                                             |
| `RecoveryChurnVsIterations.dat`       | Timeseries of path churn due to failure recovery.                                               |
| `TimeVsIterations.dat`                | Timeseries of time taken to compute the routing scheme for each TM.                             |
| `TMChurnVsIterations.dat`             | Timeseries of path churn due to recomputing routing scheme for each TM.                         |
| `TotalSinkThroughputVsIterations.dat` | Timeseries of total throughput to the sink node in case of flash crowds.                        |
| `TotalThroughputVsIterations.dat`     | Timeseries of normalized throughput (total traffic delivered / total demand for all node pairs).|


Note that each file will have data for all TE systems invoked during
simulation. The data will be concatenated and tagged with the name of the TE
system used.  Usually, each file is in TSV format, and the header in each file
will describe the contents in each column.
