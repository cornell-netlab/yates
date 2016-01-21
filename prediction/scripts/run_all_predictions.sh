#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
Graph_DIR=${CURR_DIR}/graphs
HOST_DIR=${CURR_DIR}/../data/hosts/zoo
TOPO_DIR=${CURR_DIR}/topo-zoo
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
    if [ $n -gt 50 ]
    then
        echo "big.. $n"
    else
        echo "small.. $n"
        ./synthesize/bin/synthesize -n ${prefix} -r 200 -m ${n} -t ${Graph_DIR}/${prefix}.txt --merge_len 12 &
    fi
done

