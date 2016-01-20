#!/bin/bash
set -x
topology=$1
data_dir=$2
TOPO_DIR=data/topologies
for scheme in spf ksp ecmp mcf mwmcf raeke semimcfecmp semimcfksp semimcfmcf semimcfraeke semimcfvlb vlb;
    do
        python simulate/viz/showEdgeCongestion.py $scheme ${TOPO_DIR}/${topology}.dot expData/${data_dir}/EdgeCongestionVsIterations.dat simulation
        python simulate/viz/showEdgeCongestion.py $scheme ${TOPO_DIR}/${topology}.dot expData/${data_dir}/EdgeExpCongestionVsIterations.dat expected
done

python simulate/viz/CongestionVsIterations.py ${data_dir} Max
python simulate/viz/CongestionVsIterations.py ${data_dir} Mean
python simulate/viz/CongestionVsIterations.py ${data_dir} k50
python simulate/viz/CongestionVsIterations.py ${data_dir} k90
python simulate/viz/CongestionVsIterations.py ${data_dir} k95

python simulate/viz/TotalThroughputVsIterations.py ${data_dir}
python simulate/viz/LatencyCDF.py expData/${data_dir}/LatencyDistributionVsIterations.dat
python simulate/viz/LossVsIterations.py ${data_dir} Failure
python simulate/viz/LossVsIterations.py ${data_dir} Congestion
python simulate/viz/NumPathsVsIterations.py ${data_dir}
python simulate/viz/ChurnVsIterations.py ${data_dir} TM
python simulate/viz/ChurnVsIterations.py ${data_dir} Recovery
python simulate/viz/TimeVsIterations.py ${data_dir}
