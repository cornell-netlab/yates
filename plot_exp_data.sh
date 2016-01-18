#!/bin/bash
set -x
topology=$1
TOPO_DIR=data/topologies
for scheme in spf ksp ecmp mcf mwmcf raeke semimcfecmp semimcfksp semimcfmcf semimcfraeke semimcfvlb vlb;
    do
        python simulate/viz/showEdgeCongestion.py $scheme ${TOPO_DIR}/${topology}.dot expData/${topology}/EdgeCongestionVsIterations.dat simulation
        python simulate/viz/showEdgeCongestion.py $scheme ${TOPO_DIR}/${topology}.dot expData/${topology}/EdgeExpCongestionVsIterations.dat expected
done

python simulate/viz/CongestionVsIterations.py ${topology} Max
python simulate/viz/CongestionVsIterations.py ${topology} Mean
python simulate/viz/CongestionVsIterations.py ${topology} k50
python simulate/viz/CongestionVsIterations.py ${topology} k90
python simulate/viz/CongestionVsIterations.py ${topology} k95

python simulate/viz/TotalThroughputVsIterations.py ${topology}
python simulate/viz/LatencyCDF.py expData/${topology}/LatencyDistributionVsIterations.dat
python simulate/viz/LossVsIterations.py ${topology} Failure
python simulate/viz/LossVsIterations.py ${topology} Congestion
python simulate/viz/NumPathsVsIterations.py ${topology}
