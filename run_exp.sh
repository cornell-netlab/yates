#!/bin/bash
for b in 3 5 ; do
    for s in 1 2 3 ; do
        ./run_failures.sh $b $s &
    done
done
./run_burst.sh AttMpls &
./run_burst.sh Sprint &
./run_burst.sh Geant2012 &

./run_prediction_test.sh AttMpls &
./run_prediction_test.sh Sprint &
./run_prediction_test.sh Geant2012 &
