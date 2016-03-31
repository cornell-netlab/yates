#!/bin/bash

# kulfi_path="~/workspace/kulfi"
# host_path=${kulfi_path}"/data/hosts"
# topo_path=${kulfi_path}"/data/topologies"
# demand_path="/home/abilene/demand_data"

# num_tm=24
# budget="-budget {3}"
# out_dir="v2_budget_{2}_scale_{1}_budget_{3}_numtm_${num_tm}"

# exec="${kulfi_path}/Simulate_Driver.native"
# args="-all ${topo_path}/{2}.dot ${demand_path}/{2}-matrix/{2}_mergelen_12 ${demand_path}/{2}-matrix/{2}_mergelen_12  ${host_path}/{2}.hosts ${num_tm} -deloop -scalesyn ${budget} -scale {1} -out ${out_dir}"


# ${exec} ${args} ::: 1 2 4 8 ::: abilene_hourly AttMpls Sprint Geant2012 Globalcenter BtNorthAmerica Janetbackbone Ntt Uunet ::: 1 3 5 7 10


networks=("AttMpls" "BtNorthAmerica" "Geant2012" "Globalcenter" "Janetbackbone" "Ntt" "Sprint" "Uunet" "abilene_hourly")
methods=(0 1 2)

for f in "${networks[@]}"
do
  for m in "${methods[@]}"
  do
    echo "./Simulate_Driver.native -mcf data/topologies/${f}.dot ./prediction/matrix/${f}-matrix/${f}_mergelen_12 ./prediction/matrix/${f}-matrix/${f}_mergelen_12  data/hosts/${f}.hosts 1 -gurobi-method ${m}"
  done
done
