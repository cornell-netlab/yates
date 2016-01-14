import pygraphviz as pgv

# read hosts file
host_to_id = dict()
id = 0
with open("hosts.txt") as h:
    for x in h.readlines():
        host_name = x[:-1]
        host_to_id[host_name] = id
        id += 1

# read dot file
g = pgv.AGraph('../data/topologies/abilene.dot')
for n in g.iternodes():
    print n
    if n not in host_to_id:
        host_to_id[n] = id
        id += 1

# output
for k,v in host_to_id.iteritems():
    print k,v

with open("custom_topo.txt", "w") as f:
    f.write(str(g.number_of_nodes())+"\n")
    f.write(str(g.number_of_edges())+"\n")

    for e in g.iteredges():
        u,v = e
        f.write(str(host_to_id[u]) + " " + str(host_to_id[v]) + " 1.0\n")



