#!/bin/bash
GREEN='\033[00;32m'
RESTORE='\033[0m'

mkdir -p synthetic_demands
cp ../pareto ./
cp ../patterns ./
for i in `cat num_hosts.txt`
do
echo -en $GREEN "================= N = $i ============= " $RESTORE
../libpredict/bin/predict 2 1000 $i synthetic_demands/syn-$i 1.0
done

rm pareto patterns
