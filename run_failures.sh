#!/bin/bash
set -x
budget="-budget $1"
num_tm=2
failure="-fail-time 0"
local_recovery="-lr-delay 1"
global_recovery="-gr-delay 2"
scale="-scale $2"

topologies="AttMpls Sprint Geant2012"

for topo in $topologies ; do
    echo $topo
    for params in \
        "${num_tm} -deloop ${budget} -scalesyn ${scale}"\
        "${num_tm} -deloop ${budget} -scalesyn ${scale} ${failure}"\
        "${num_tm} -deloop ${budget} -scalesyn ${scale} ${failure} ${local_recovery}"\
        "${num_tm} -deloop ${budget} -scalesyn ${scale} ${failure} ${local_recovery} ${global_recovery}" ; do
        echo $params
        out_dir=${topo}_$(echo "${params}" | tr '[[:space:]]' '_')
        ./Simulate_Driver.native -all data/topologies/${topo}.dot data/demands/${topo}.txt data/demands/${topo}.txt data/hosts/${topo}.hosts ${params} -out ${out_dir} >> out_${out_dir}.txt
        ./plot_exp_data.sh ${topo} ${out_dir} &
        python ./expData/web/gen_per_topo.py > ./expData/${out_dir}/index.html
    done
done

python ./expData/web/gen_results_browser.py ./expData > expData/index.html
python ./expData/web/gen_metric_browser.py ./expData
