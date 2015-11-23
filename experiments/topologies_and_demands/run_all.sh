#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
TOPO_DIR=${CURR_DIR}/data/topologies/zoo
echo $EXP_DIR 
echo $CURR_DIR
mkdir -p $EXP_DIR/results
mkdir -p $EXP_DIR/hosts
mkdir -p $CURR_DIR/expData
mkdir -p $CURR_DIR/lp
mkdir -p ~/log
TOPOLOGY_FILES=${TOPO_DIR}/Abilene.dot
TM_DIR=${CURR_DIR}/prediction/scripts/synthetic_demands
#TM_DIR=${CURR_DIR}/data/gen

for tf in $TOPOLOGY_FILES
do
    echo "$tf"
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    echo $prefix
    num_hosts=`grep "host" $tf | wc -l`
    rm -rf $CURR_DIR/expData/${prefix}
    echo $num_hosts
    actual_demand=${TM_DIR}/syn-${num_hosts}
    predicted_demand=${TM_DIR}/syn-${num_hosts}_LinearRegression
    hosts_file=${TM_DIR}/${prefix}.host
    cmd="./Simulate_Driver.native ${tf} ${actual_demand} ${predicted_demand} ${hosts_file} 100 -all"
    echo $cmd
    $cmd &> ~/log/${prefix}.log &
done

watch -n 1 "ps aux | grep \"Simulate_Driver\" | grep -v \"grep\" | sort -rk 3,3 "
