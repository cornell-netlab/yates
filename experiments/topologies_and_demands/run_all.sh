#!/bin/bash

THIS_DIR=.
PREDICT=${THIS_DIR}/prediction/predict/bin/predict
TOPOLOGY_FILES=${THIS_DIR}/data/topologies/zoo/*.dot 

for tf in $TOPOLOGY_FILES
do
    echo "$tf"
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    echo $prefix
    num_hosts=`grep mac $tf | wc -l`
    echo $num_hosts
    "${PREDICT} 2 1000 $num_hosts $prefix 1.0"

    # STEP 1: mv files to correct names
    
    # STEP 2: Call Simulator
  # ./Simulate_Driver.native data/topologies/3cycle.dot data/demands/3cycle_demands.txt data/predict/3cycle_predict.txt data/hosts/3cycle_hosts.txt 4 -all

    # STEP 3: Call Simulator
    # mv expData/CongestionVsIterations.dat expData/3cycle-CongestionVsIterations.dat
    
done
