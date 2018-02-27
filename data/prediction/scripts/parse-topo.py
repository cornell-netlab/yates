import pygraphviz as pgv

import sys

#print 'Number of arguments:', len(sys.argv), 'arguments.'
#print 'Argument List:', str(sys.argv)


# read hosts file
host_to_id = dict()
id = 0
with open(sys.argv[1]) as h:
    for x in h.readlines():
        host_name = x[:-1]
        host_to_id[host_name] = id
        id += 1

# read dot file
g = pgv.AGraph(sys.argv[2])
for n in g.iternodes():
    #print n
    if n not in host_to_id:
        host_to_id[n] = id
        id += 1

# output
#for k,v in host_to_id.iteritems():
    #print k,v

with open(sys.argv[3], "w") as f:
    f.write(str(g.number_of_nodes())+"\n")
    f.write(str(g.number_of_edges())+"\n")

    for e in g.iteredges():
        u,v = e
        f.write(str(host_to_id[u]) + " " + str(host_to_id[v]) + " 1.0\n")



