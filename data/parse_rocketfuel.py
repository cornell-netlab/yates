import networkx as nx
import os
import pickle
from string import digits
import sys
import urllib
import simplejson

googleGeocodeUrl = 'http://maps.googleapis.com/maps/api/geocode/json?'
cached_geocodes = "geocodes.save"

def geocode(query, from_sensor=False):
    query = query.encode('utf-8')
    res = None
    db = dict()
    if os.path.exists(cached_geocodes):
        db = pickle.load(open(cached_geocodes, "rb"))
        res = db.get(query)
    if res == None:
        params = {
            'address': query,
            'sensor': "true" if from_sensor else "false"
        }
        url = googleGeocodeUrl + urllib.urlencode(params)
        json_response = urllib.urlopen(url)
        response = simplejson.loads(json_response.read())
        if response['results']:
            location = response['results'][0]['geometry']['location']
            latitude, longitude = location['lat'], location['lng']
            print query, latitude, longitude
            res = (latitude, longitude)
            db[query] = res
            pickle.dump(db, open(cached_geocodes, "wb"))
    return res

def gen_dot(network_name, rf_file):
    edges = set()
    nodes = set()
    with open(rf_file) as f:
        for l in f.readlines():
            toks = l.split()
            src = toks[0].translate(None, digits)
            dst = toks[1].translate(None, digits)
            if src == dst:
                continue
            nodes.add(src)
            nodes.add(dst)
            edges.add((src, dst))
            edges.add((dst, src))

    # Export GML file
    G = nx.DiGraph()
    G.name = network_name
    for n in sorted(nodes):
        pos = geocode(n)
        G.add_node(n, Latitude = pos[0], Longitude = pos[1], Internal = 1)
    for e in edges:
        G.add_edge(e[0], e[1])
    nx.write_gml(G, 'zoo_gml/'+network_name+".gml")

    # Export DOT file
    id = 0
    city_id = dict()
    for n in G.nodes(data=True):
        city_id[n[0]] = id
        id += 1

    H = nx.DiGraph()
    H.name = network_name
    for n in G.nodes(data=True):
        id = city_id[n[0]]
        H.add_node('s'+str(id),
                   type = 'switch',
                   id = id,
                   #loc = n[0],
                   )
        H.add_node('h'+str(id),
                   type = 'host',
                   ip = "10.0.0."+str(id),
                   mac = "10:00:00:00:00:"+str(id))
        H.add_edge('s'+str(id), 'h'+str(id), capacity='1Gbps', cost=1,
                   src_port=1, dst_port=1)
        H.add_edge('h'+str(id), 's'+str(id), capacity='1Gbps', cost=1,
                   src_port=1, dst_port=1)
    for e in G.edges(data=True):
        src = e[0]
        dst = e[1]
        src_id = city_id[src]
        dst_id = city_id[dst]
        H.add_edge('s'+str(src_id), 's'+str(dst_id), capacity='1Gbps', cost=1,
                   src_port = dst_id+100, dst_port=src_id+100)
    nx.drawing.nx_pydot.write_dot(H, 'topologies/'+network_name+".dot")

    # Export host file
    with open('hosts/'+network_name+'.hosts', 'w') as f:
        for n in H.nodes(data=True):
            if n[1]['type'] == 'host':
                f.write(n[0] + '\n')

if __name__ == "__main__":
    network_name = sys.argv[1]
    rf_file = sys.argv[2]
    gen_dot(network_name, rf_file)
