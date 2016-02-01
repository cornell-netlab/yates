#!/bin/bash

kulfi_path="/home/abilene/src/git/kulfi"
host_path=${kulfi_path}"/data/hosts"
topo_path=${kulfi_path}"/data/topologies"
demand_path="/home/abilene/demand_data"

num_tm=24
budget=3
fail="-fail-time 0 -lr-delay 10 -fail-num {3}"
fail_name=$(echo "${fail}" | tr '[[:space:]]' '_')
out_dir="v3_fail_{2}_scale_{1}_budget_${budget}_numtm_${num_tm}_${fail_name}"

executable="${kulfi_path}/Simulate_Driver.native"
args="-all ${topo_path}/{2}.dot ${demand_path}/{2}-matrix/{2}_mergelen_12 ${demand_path}/{2}-matrix/{2}_mergelen_12  ${host_path}/{2}.hosts ${num_tm} -deloop -scalesyn -budget ${budget} -scale {1} ${fail} -out ${out_dir}"


parallel -vv -S 16/atlas-1,16/atlas-2,16/atlas-3,16/atlas-4,16/atlas-5,16/atlas-6,16/atlas-7,16/atlas-8,16/atlas-9,16/atlas-10,16/atlas-11,16/atlas-12,16/atlas-14,16/atlas-15,16/atlas-16,16/atlas-17,16/atlas-18,16/atlas-19,16/atlas-20,16/atlas-21,16/atlas-22,16/atlas-23 ${executable} ${args} ::: 0.5 1 1.5 2 2.5 3 3.5 4 ::: abilene_hourly AttMpls Sprint Geant2012 Globalcenter BtNorthAmerica Janetbackbone Ntt Uunet ::: 0 1 2 3


