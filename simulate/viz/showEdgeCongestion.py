import json
import networkx as nx
from networkx.readwrite import json_graph
import pygraphviz as pgv
import sys

def congestion_to_color(c):
    colors = ['/rdylgn6/5','/rdylgn6/4','/rdylgn6/3','/rdylgn6/2']
    if c == 0:
        return '#aaaaaa'
    elif c < 1:
        return colors[int(c * len(colors))]
    elif c < 2:
        return '/reds9/7'
    elif c < 3:
        return '/reds9/8'
    else:
        return '/reds9/9'

def display (scheme, topology, all_congestions, directory, expected):
    G = pgv.AGraph(topology)
    for e in G.edges():
        link = '('+e[0]+','+e[1]+')'
        max_cong = max(get_link_congestion(all_congestions, scheme, link))
        e.attr['color'] = congestion_to_color(max_cong)
        e.attr['congestion'] = max_cong
        e.attr['label'] = int(max_cong * 100)/100.0
        if max_cong > 1:
            e.attr['weight'] = 10
    nxg = nx.from_agraph(G)
    data = json_graph.node_link_data(nxg)
    s = json.dump(data, open('test.json', 'w'))
    G.layout()
    if expected:
        G.draw(directory+'/link_cong_exp_'+scheme+'.svg')
    else:
        G.draw(directory+'/link_cong_'+scheme+'.svg')

def parse_congestions_file (filename):
    all_congestions = dict()
    scheme = ""
    iteration = ""
    links = dict()
    with open(filename) as f:
        for line in f.readlines():
            if line[0] == "#":
                continue
            else:
                tokens = line.split('\t')
                if len(tokens) != 3:
                    if len(links) != 0:
                        all_congestions[scheme][int(iteration)] = links
                        links = dict()
                    continue
                if tokens[2] == '\n':
                    if len(links) != 0:
                        all_congestions[scheme][int(iteration)] = links
                        links = dict()
                    scheme = tokens[0]
                    iteration = tokens[1]
                    if scheme not in all_congestions.keys():
                        all_congestions[scheme] = dict()
                else:
                    edge_cr = tokens[2].split(':')
                    links[edge_cr[0].strip()] = float(edge_cr[1].strip())
    return all_congestions

def get_link_congestion (all_congestions, scheme, link):
    scm_congestion = all_congestions[scheme]
    link_congestion = []
    for iteration, congs in scm_congestion.iteritems():
        link_congestion.append(congs.get(link, 0))
    return link_congestion


if __name__ == "__main__":
    if len(sys.argv) < 5:
        print "Usage: " + sys.argv[0] + " <scheme_name> <topology_file> <edge_congestion_file> <simulation/expected>"
    else:
        scheme = sys.argv[1]
        topology = sys.argv[2]
        directory='/'.join(sys.argv[3].split('/')[:-1])
        all_congestions = parse_congestions_file(sys.argv[3])
        if sys.argv[4] == 'expected':
            expected=True
        else:
            expected=False
        display(scheme, topology, all_congestions, directory, expected)
