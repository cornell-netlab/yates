import numpy as np
import os
import operator
import sys
from collections import defaultdict
from itertools import chain, combinations

import matplotlib.pyplot as pp
import CommonConf
BASE = 'ksp'
algs = ['mcf', 'raeke', 'edksp', 'vlb', 'ksp', 'ecmp']

def parse_path_file(paths_file):
    src,dst = '',''
    paths = dict()
    with open(paths_file) as f:
        for l in f.readlines():
            if "->" in l:
                src = l.split()[0]
                dst = l.split()[2]
                paths[(src,dst)] = dict()
            else:
                if len(l.strip()) == 0:
                    continue
                path = tuple(l.split('@')[0].strip()[1:-1].split(', '))
                weight = l.split('@')[1].strip()
                paths[(src,dst)][path] = float(weight)
    return paths

def powerset(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def find_min_hitting_set(paths):
    all_edges = set()
    for p in paths:
        for e in p:
            if 'h' not in e:
                all_edges.add(e)
    #print sorted(all_edges)
    for s in powerset(all_edges):
        s = set(s)
        valid = True
        for p in paths:
            if len(s & p) == 0:
                valid = False
                break
        if valid:
            return s

def freq(l):
    h = dict()
    for x in l:
        h[x] = h.get(x, 0) + 1
    return h

def plot_resilience(paths_dir, out_file):
    fig = pp.figure(figsize=(12,8))
    ax = fig.add_subplot(111)
    paths_file = dict()
    resilience = dict()
    rects = dict()
    width = 0.9/len(algs)
    CommonConf.setupMPPDefaults()
    colors = CommonConf.getLineColorsDict()
    bar_width = 0.7
    for alg in algs:
        paths_file[alg] = paths_dir + '/' + alg + '_0'
    for alg in algs:
        mhs_vals = []
        paths = parse_path_file(paths_file[alg])
        for sd,sdpaths in paths.iteritems():
            mhs = find_min_hitting_set([set(p) for p in sdpaths.keys()])
            #print len(sdpaths), len(mhs)
            mhs_vals.append(len(mhs)-1)
        resilience[alg] = freq(mhs_vals)

    t = [max(resilience[alg].keys()) for alg in algs]
    xs = np.arange(max(t)+1)
    i = 0
    for alg in algs:
        ys = []
        for x in xs:
            ys.append(np.sum([resilience[alg].get(X,0) for X in xs if X >= x]))
        print alg, ys
        rects[alg] = ax.bar(xs-0.5+i*width, ys, width, color=colors[alg],
                            edgecolor=None, alpha=0.8, zorder=1/np.mean(ys))
        i = i+1

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    pp.xticks(range(len(algs)), [' ' for x in algs])
    ax.locator_params(axis='y', nbins=6)
    ax.set_ylabel('Number of f-resilient node pairs')
    ax.set_xticklabels(xs)
    ax.legend([rects[x] for x  in algs],
              [CommonConf.gen_label(x) for x in algs],
              loc='best', borderaxespad=1., fancybox=True)
    pp.xlim(0.5,2.5)
    pp.tight_layout()
    pp.savefig("resilience.pdf")


def test_mhs():
    for s in [['abc', 'acd', 'cef', 'bef'], ['125','234','31'], ['145', '238', '67', '137']]:
        sets = [set(x) for x in s]
        print find_min_hitting_set(sets)

if __name__ == "__main__":
    if len(sys.argv) < 5:
        print "Usage: " + sys.argv[0] + " rtt_file paths_dir out_file rtt/hop"
    paths_dir = sys.argv[1]
    out_file = sys.argv[2]
    #test_mhs()
    plot_resilience(paths_dir, out_file)
