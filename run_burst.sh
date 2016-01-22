#!/bin/bash
set -x
topo_name=$1 #=grid5h1EvenDemand
pred_dir="prediction/matrix/"
budget=3
scale=2

mode=pair
for amount in 0.03 0.06 0.09 0.12 0.15 0.18 0.21 0.24 0.27 0.30 0.33 0.36 0.39 0.42 0.45 0.48 0.51 0.54 0.57 0.60
#for amount in 0.03
do
    echo $amount
    ./Simulate_Driver.native -all data/topologies/${topo_name}.dot ${pred_dir}/${topo_name}-matrix/${topo_name}_mergelen_12_burst_${mode}_${amount} ${pred_dir}/${topo_name}-matrix/${topo_name}_mergelen_12 data/hosts/${topo_name}.hosts 4 -budget ${budget} -deloop -scalesyn -scale $scale -out ${topo_name}_burst_${mode}_${amount} &
done

for j in `jobs -p` ; do
    wait $j || echo "fail"
done

python simulate/viz/PerformanceVsBurst.py ${topo_name}_burst_${mode}
