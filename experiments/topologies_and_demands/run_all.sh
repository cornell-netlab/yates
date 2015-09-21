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
done
