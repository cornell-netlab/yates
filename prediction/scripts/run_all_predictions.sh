#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
Graph_DIR=${CURR_DIR}/graphs
HOST_DIR=${CURR_DIR}/../data/hosts/zoo
TOPO_DIR=${CURR_DIR}/../data/topologies/zoo
mkdir $HOST_DIR
echo $EXP_DIR
echo $CURR_DIR
echo $HOST_DIR
echo $TOPO_DIR

for tf in $TOPO_DIR/*
do
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base

    n=`grep "host" $tf | wc -l`
    echo $n
    ./synthesize/bin/synthesize -n ${prefix} -r 100 -m ${n} -t ${Graph_DIR}/${prefix}.txt 
done

