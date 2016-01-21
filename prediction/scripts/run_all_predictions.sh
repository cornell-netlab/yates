#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
Graph_DIR=${CURR_DIR}/graphs
TOPO_DIR=${CURR_DIR}/../data/topologies
echo $EXP_DIR
echo $CURR_DIR
echo $TOPO_DIR

for tf in $TOPO_DIR/*.dot
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
        #./synthesize/bin/synthesize -n ${prefix} -r 200 -m ${n} -t ${Graph_DIR}/${prefix}.txt &
        ./synthesize/bin/synthesize -n ${prefix} -r 200 -m ${n} -t ${Graph_DIR}/${prefix}.txt --merge_len 12 
        #./synthesize/bin/synthesize -n ${prefix} -r 200 -m ${n} -t ${Graph_DIR}/${prefix}.txt &
        #./synthesize/bin/synthesize -n ${prefix} -r 200 -m ${n} -t ${Graph_DIR}/${prefix}.txt &
    fi
done

