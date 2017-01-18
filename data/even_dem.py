#!/usr/bin/python
import sys
dem = []
dem_str = ""
nh=int(sys.argv[1])
for tm in range(10):
    row = []
    for src in range(nh):
        for dst in range(nh):
            if src == dst:
                row.append(0.0)
            else:
                row.append((2**30))
    dem.append(row)

for row in dem:
    print " ".join(str(x) for x in row)

