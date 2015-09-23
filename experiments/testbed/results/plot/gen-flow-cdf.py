#!/usr/bin/env python

fs = []
flag = True
with open("formatted-flows.txt", 'r') as f:
    for x in f.readlines():
        if flag:
            flag = False
        else:
            f_size = x.split()[5].strip()
            fs.append(int(f_size))
fs.sort()
num_flows = len(fs)
cumul = 0
for f in fs:
    cumul += 1.0/num_flows
    print f, cumul
 
