#!/usr/bin/env python

demands = []
with open('X01-36', 'r') as dem_file:
    for line in dem_file.readlines():
	# convert ro bps
        demands.append([float(x)*8/3 for x in line.split()])

real_od = []
gravity = []

for row in demands:
    real_od.append(row[0:][::5])
    gravity.append(row[1:][::5])

with open('X01-36.observed', 'w') as obs_file:
    for row in real_od:
        obs_file.write(' '.join([str(x) for x in row]) + '\n')

with open('X01-36.gravity', 'w') as grav_file:
    for row in gravity:
        grav_file.write(' '.join([str(x) for x in row]) + '\n')

