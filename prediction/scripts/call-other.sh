#!/bin/bash
GREEN=$(echo -en '\033[00;32m') 
RESTORE=$(echo -en '\033[0m')

mkdir -p synthetic_demands
cp ../pareto ./
cp ../patterns ./
for i in `cat num_hosts.txt`
echo $GREEN "================= N = $i ============= " $RESTORE
do
../libpredict/bin/predict 2 1000 $i synthetic_demands/syn-$i 1.0
done

rm pareto patterns
