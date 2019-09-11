#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
Graph_DIR=graphs
HOST_DIR=${CURR_DIR}/../hosts
TOPO_DIR=${CURR_DIR}/../topologies
echo $EXP_DIR
echo $CURR_DIR
echo $HOST_DIR
echo $TOPO_DIR
exit
for tf in $TOPO_DIR/*.dot
do
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    python $EXP_DIR/parse-topo.py $HOST_DIR/${prefix}.hosts $tf ${Graph_DIR}/${prefix}.txt
done

