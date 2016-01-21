#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
Graph_DIR=graphs
HOST_DIR=${CURR_DIR}/../data/hosts
TOPO_DIR=${CURR_DIR}/../data/topologies
mkdir $HOST_DIR
echo $EXP_DIR
echo $CURR_DIR
echo $HOST_DIR
echo $TOPO_DIR

for tf in $TOPO_DIR/*.dot
do
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    python $EXP_DIR/parse-topo.py $HOST_DIR/${prefix}.hosts $tf ${Graph_DIR}/${prefix}.txt
done

