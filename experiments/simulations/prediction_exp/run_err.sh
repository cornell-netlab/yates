#!/bin/bash

kulfi_path="/home/abilene/src/git/kulfi"
host_path=${kulfi_path}"/data/hosts"
topo_path=${kulfi_path}"/data/topologies"
demand_path="/home/abilene/demand_data"

num_tm=24
budget=3
out_dir="v3_err_{2}_scale_{1}_budget_${budget}_numtm_${num_tm}_pred_err_{3}"

executable="${kulfi_path}/Simulate_Driver.native"
args="-all ${topo_path}/{2}.dot ${demand_path}/{2}-matrix/{2}_mergelen_12 ${demand_path}/{2}-matrix/{2}_mergelen_12_corr_noise_{3}  ${host_path}/{2}.hosts ${num_tm} -deloop -scalesyn -budget ${budget} -scale {1} -out ${out_dir}"


parallel -vv -S 16/atlas-1,16/atlas-2,16/atlas-3,16/atlas-4,16/atlas-5,16/atlas-6,16/atlas-7,16/atlas-8,16/atlas-9,16/atlas-10,16/atlas-11,16/atlas-12,16/atlas-14,16/atlas-15,16/atlas-16,16/atlas-17,16/atlas-18,16/atlas-19,16/atlas-20,16/atlas-21,16/atlas-22,16/atlas-23 ${executable} ${args} ::: 2 3 4 ::: abilene_hourly AttMpls Sprint Geant2012 Globalcenter BtNorthAmerica Janetbackbone Ntt Uunet ::: 0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90


