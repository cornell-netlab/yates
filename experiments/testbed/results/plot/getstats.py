import numpy as np

f = open("all-switch-load-timeline.txt")
# skip over header info
X = np.loadtxt(f)
for row in X:
    undir = [sum(row[i:i+1]) for i in range(0, len(row)-1, 2)]
    mx = np.max(undir)
    mdn = np.median(undir)
    print str(min(1.0, mx/125000000))  + " " + str(mdn/125000000)
