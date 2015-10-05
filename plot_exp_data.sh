#!/bin/bash
set -x
topology=$1
for x in spf akecmp akmcf akraeke akvlb ecmp mcf mwmcf raeke semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb ;
    do python simulate/viz/showEdgeCongestion.py $x data/topologies/zoo/${topology}.dot expData/${topology}/EdgeCongestionVsIterations.dat
done

python simulate/viz/CongestionVsIterations.py ${topology} Max
python simulate/viz/CongestionVsIterations.py ${topology} Mean
python simulate/viz/CongestionVsIterations.py ${topology} k50

python simulate/viz/CongestionVsIterations.py ${topology} Max ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
python simulate/viz/CongestionVsIterations.py ${topology} Mean ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
python simulate/viz/CongestionVsIterations.py ${topology} k50 ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
