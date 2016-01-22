#!/bin/bash
for b in 3 5 ; do
    for s in 1 2 3 ; do
        ./run.sh $b $s &
    done
done
