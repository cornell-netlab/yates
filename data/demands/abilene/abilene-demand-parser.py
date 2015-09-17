#!/usr/bin/env python

demands = []
with open('X01-36', 'r') as dem_file:
    for line in dem_file.readlines():
        demands.append(line.split())

real_od = []
gravity = []

for row in demands:
    real_od.append(row[0:][::5])
    gravity.append(row[1:][::5])

with open('X01-36.observed', 'w') as obs_file:
    for row in real_od:
        obs_file.write(' '.join(row) + '\n')

with open('X01-36.gravity', 'w') as grav_file:
    for row in gravity:
        grav_file.write(' '.join(row) + '\n')

