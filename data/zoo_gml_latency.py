#! /usr/bin/env python

import networkx as nx
import numpy as np
import os
import sys

from math import sin, cos, sqrt, atan2, radians

def main():
    net_file = sys.argv[1]
    G = nx.read_gml(net_file, label='id')
    topo_file = net_file.split('/')[-1].split('.')[0]
    # Reduce to undirected single edge graph for simplicity
    G = nx.Graph(G)

    geocoded_cities = [ n for n, data in G.nodes(data = True)
                if 'Latitude' in data and 'Longitude' in data]
    non_geocoded_cities = [n for n in G.nodes()
                           if (n not in geocoded_cities)]
    print "Non-geocoded:", non_geocoded_cities
    rf = open(topo_file+'.rtt', 'w')
    for n in non_geocoded_cities:
        if G.degree(n) < 1:
            # Single node eg ---0
            G.remove_node(n)
        elif G.degree(n) > 1:
            # Infer coordinates as mean of neighbors
            he_lats=[]
            he_lons=[]
            for neigh in G.neighbors(n):
                if 'Latitude' in G.node[neigh]:
                    he_lats.append(G.node[neigh]['Latitude'])
                if 'Longitude' in G.node[neigh]:
                    he_lons.append(G.node[neigh]['Longitude'])
            if len(he_lats) > 0 and len(he_lons) > 0:
                G.node[n]['Latitude'] = np.mean(he_lats)
                G.node[n]['Longitude'] = np.mean(he_lons)
            print G.node[n]

    for src, dst, data in G.edges(data=True):
        #print src, dst, G.node[src], G.node[dst]
        lon1 = G.node[src]['Longitude']
        lat1 = G.node[src]['Latitude']
        lon2 = G.node[dst]['Longitude']
        lat2 = G.node[dst]['Latitude']

        # infer latency
        r_lon1 = radians(lon1)
        r_lat1 = radians(lat1)
        r_lon2 = radians(lon2)
        r_lat2 = radians(lat2)
        dlon = r_lon2 - r_lon1
        dlat = r_lat2 - r_lat1
        a = sin(dlat / 2)**2 + cos(r_lat1) * cos(r_lat2) * sin(dlon / 2)**2
        c = 2 * atan2(sqrt(a), sqrt(1 - a))
        R = 6373.0
        distance = R * c            # in km
        latency = distance / 100    # RTT km / 2e5 kmps * 1e3 ms/s * 2
        if latency == 0:
            latency = 0.5
        link = '(s'+str(src)+',s'+str(dst)+')'
        rlink = '(s'+str(dst)+',s'+str(src)+')'
        print G.node[src]['label'], G.node[dst]['label'], link, latency
        rf.write(link + " " + str(latency) + "\n")
        rf.write(rlink + " " + str(latency) + "\n")
    rf.close()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
