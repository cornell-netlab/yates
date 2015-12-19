#!/bin/bash
set -x
topology=$1
TOPO_DIR=data/gen
for scheme in spf ksp akecmp akksp akmcf akraeke akvlb aktestscheme ecmp mcf mwmcf raeke semimcfecmp semimcfksp semimcfmcf semimcfraeke semimcfvlb semimcftestscheme vlb testscheme;
    do python simulate/viz/showEdgeCongestion.py $scheme ${TOPO_DIR}/${topology}.dot expData/${topology}/EdgeCongestionVsIterations.dat
done

python simulate/viz/CongestionVsIterations.py ${topology} Max
python simulate/viz/CongestionVsIterations.py ${topology} Mean
python simulate/viz/CongestionVsIterations.py ${topology} k50

python simulate/viz/CongestionVsIterations.py ${topology} Max ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
python simulate/viz/CongestionVsIterations.py ${topology} Mean ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
python simulate/viz/CongestionVsIterations.py ${topology} k50 ecmp mcf semimcfecmp semimcfmcf semimcfraeke semimcfvlb vlb
