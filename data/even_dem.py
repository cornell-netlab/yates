#!/usr/bin/python
dem = []
dem_str = ""
nh=18
for tm in range(10):
    row = []
    for src in range(nh):
        for dst in range(nh):
            if src == dst:
                row.append(0.0)
            else:
                row.append((2**30)/(nh-1))
    dem.append(row)

for row in dem:
    print " ".join(str(x) for x in row)

