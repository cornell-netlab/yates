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
TOPOLOGY_FILES=${TOPO_DIR}/*
TM_DIR=${CURR_DIR}/prediction/scripts/synthetic_demands

for tf in $TOPOLOGY_FILES
do
    echo "$tf"
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    echo $prefix
    num_hosts=`grep mac $tf | wc -l`
    rm -rf $CURR_DIR/expData/${prefix}
    echo $num_hosts
    actual_demand=${TM_DIR}/syn-${num_hosts}
    predicted_demand=${TM_DIR}/syn-${num_hosts}_LinearRegression
    sed ':a;N;$!ba;s/\n/ /g' ${tf} | sed 's/\;/\n/g' | sed 's/{/\n/g' | grep host | awk '{print $1;}' | sort > $EXP_DIR/hosts/${prefix}.txt
    cmd="./Simulate_Driver.native -scalesyn ${tf} ${actual_demand} ${predicted_demand} $EXP_DIR/hosts/${prefix}.txt 1000 -all"
    echo $cmd
    $cmd &> ~/log/${prefix}.log &
done

watch -n 1 "ps aux | grep \"Simulate_Driver\" | grep -v \"grep\" | sort -rk 3,3 "
