#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
TOPO_DIR=${CURR_DIR}/data/topologies/zoo
echo $EXP_DIR 
echo $CURR_DIR
mkdir -p $EXP_DIR/results
mkdir -p $EXP_DIR/hosts
PREDICT=${CURR_DIR}/prediction/libpredict/bin/predict
TOPOLOGY_FILES=${TOPO_DIR}/Vinaren.dot 
TM_DIR=${CURR_DIR}/prediction/scripts/synthetic_demands

for tf in $TOPOLOGY_FILES
do
    echo "$tf"
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    echo $prefix
    num_hosts=`grep mac $tf | wc -l`
    echo $num_hosts
    actual_demand=${TM_DIR}/syn-${num_hosts}
    predicted_demand=${TM_DIR}/syn-${num_hosts}_LinearRegression_riskAverse
    sed ':a;N;$!ba;s/\n/ /g' ${tf} | sed 's/\;/\n/g' | sed 's/{/\n/g' | grep host | awk '{print $1;}' | sort > $EXP_DIR/hosts/${prefix}.txt
    cmd="./Simulate_Driver.native ${tf} ${actual_demand} ${predicted_demand} $EXP_DIR/hosts/${prefix}.txt 100 -all"
    echo $cmd
    $cmd
    mkdir -p $EXP_DIR/results/${prefix}
    mv ${CURR_DIR}/expData/* ${EXP_DIR}/results/${prefix}/
   # STEP 1: mv files to correct names
    
    # STEP 2: Call Simulator
  # ./Simulate_Driver.native data/topologies/3cycle.dot data/demands/3cycle_demands.txt data/predict/3cycle_predict.txt data/hosts/3cycle_hosts.txt 4 -all

    # STEP 3: Call Simulator
    # mv expData/CongestionVsIterations.dat expData/3cycle-CongestionVsIterations.dat
    
done
