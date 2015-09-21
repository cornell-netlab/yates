#!/bin/bash

for i in {1..93}
do
./libpredict/bin/predict 2 1000 $i matrix/syn-$i 1.0
done

for i in 110 113 125 127 145 149 153 158 193 197 754
do
./libpredict/bin/predict 2 1000 $i matrix/syn-$i 1.0
done
