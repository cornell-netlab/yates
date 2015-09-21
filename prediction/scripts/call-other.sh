#!/bin/bash

mkdir -p synthetic_demands
cp ../pareto ./
cp ../patterns ./
for i in `cat num_hosts.txt | head -n 1`
do
../bin/predict 2 1000 $i synthetic_demands/syn-$i 1.0
done

rm pareto patterns
