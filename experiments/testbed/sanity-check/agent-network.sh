#!/bin/bash

for i in `seq 1 12`; do
    ping -c 1 10.0.0.$i
done
