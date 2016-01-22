#!/bin/bash

topo_name=$1 #=grid5h1EvenDemand
pred_dir="prediction/matrix/"
budget=3
scale=2

for err in 0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00
#for err in 0.00 0.50 1.00 1.50 2.00 2.50 3.00 3.50 4.00 4.50 5.00
do
    echo $err
    ./Simulate_Driver.native -all data/topologies/${topo_name}.dot ${pred_dir}/${topo_name}-matrix/${topo_name}_mergelen_12 ${pred_dir}/${topo_name}-matrix/${topo_name}_mergelen_12_error_${err} data/hosts/${topo_name}.hosts 10 -deloop -scalesyn -scale $scale -budget $budget -out ${topo_name}${err} &
done

for j in `jobs -p` ; do
    wait $j || echo "fail"
done
python simulate/viz/prediction_resilience.py ${topo_name}
