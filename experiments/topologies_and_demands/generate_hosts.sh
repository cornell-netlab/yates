#!/bin/bash

EXP_DIR=$( cd "$( dirname "$0" )" && pwd )
CURR_DIR=$( pwd )
TOPO_DIR=${CURR_DIR}/data/topologies
HOST_DIR=${CURR_DIR}/data/hosts
mkdir $HOST_DIR
echo $EXP_DIR
echo $CURR_DIR

for tf in $TOPO_DIR/*.dot
do
    base=${tf##*/}
    prefix=${base%.dot}
    echo $base
    num_hosts=`grep "host" $tf | wc -l`
    #echo "                ++"$num_hosts
    tail -n +2 $tf | tr -d '\n'  > $EXP_DIR/output 
    sed -i 's/;/\n/g' $EXP_DIR/output
    sed -i 's/\[/ \[/g' $EXP_DIR/output
    grep "host" $EXP_DIR/output | awk '{print $1}' > $HOST_DIR/${prefix}.hosts
    #echo -e "                ++\c"
    #grep "h" $HOST_DIR/${prefix}.hosts | wc -l
    #echo "--------------------------------------------------"
    #cat $HOST_DIR/${prefix}.hosts
    #echo "--------------------------------------------------"
    #read -n1 -r -p "Press any key to continue..." key
done
rm $EXP_DIR/output

