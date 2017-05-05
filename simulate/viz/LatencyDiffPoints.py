import os
import operator
import sys
from collections import defaultdict

import matplotlib.pyplot as pp
import CommonConf
BASE = 'ksp'
algs = ['mcf', 'raeke', 'edksp', 'vlb', 'ksp', 'ecmp']

def setupMPPDefaults():
    pp.rcParams['font.size'] = 66
    pp.rcParams['mathtext.default'] = 'regular'
    pp.rcParams['ytick.labelsize'] = 62
    pp.rcParams['xtick.labelsize'] = 62
    pp.rcParams['legend.fontsize'] = 62
    pp.rcParams['lines.markersize'] = 12
    pp.rcParams['axes.titlesize'] = 60
    pp.rcParams['axes.labelsize'] = 60
    pp.rcParams['axes.edgecolor'] = 'grey'
    pp.rcParams['axes.linewidth'] = 3.0
    pp.rcParams['axes.grid'] = True
    pp.rcParams['grid.alpha'] = 0.4
    pp.rcParams['grid.color'] = 'grey'
    pp.rcParams['legend.frameon'] = True
    pp.rcParams['legend.framealpha'] = 0.4
    pp.rcParams['legend.numpoints'] = 1
    pp.rcParams['legend.scatterpoints'] = 1


def parse_rtt_file(rtt_file):
    rtts = dict()
    with open(rtt_file) as f:
        for l in f.readlines():
            tokens = l.split()
            rtts[tokens[0]] = float(tokens[1])
    return rtts

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

def plot_latency(rtt_file, paths_dir, out_file, rtt_hop):
    paths_file = dict()
    for alg in algs:
        paths_file[alg] = paths_dir + '/' + alg + '_0'
    rtts = parse_rtt_file(rtt_file)
    alg_sd_wtrtt = defaultdict(dict) # alg -> sd -> weighted_rtt
    for alg in algs:
        paths = parse_path_file(paths_file[alg])
        for sd,sdpaths in paths.iteritems():
            weighted_rtts = 0
            for path,weight in sdpaths.iteritems():
                path_rtt = 0
                if rtt_hop == 'rtt':
                    for link in path:
                        path_rtt += rtts.get(link, 0)
                else:
                    path_rtt = len(path) - 2
                weighted_rtts += weight * path_rtt
            alg_sd_wtrtt[alg][sd] = weighted_rtts
     # sort hosts by ecmp weighted RTT
    sorted_sd = sorted(alg_sd_wtrtt['ecmp'].items(), key=operator.itemgetter(1))
    sorted_sd = [x[0] for x in sorted_sd]
    alg_sorted_lats = dict() # alg -> list of latencies sorted by ecmp distance
    for alg in algs:
        if alg == BASE:
            alg_sorted_lats[alg] = [alg_sd_wtrtt[BASE][sd] for sd in sorted_sd]
        else:
            alg_sorted_lats[alg] = [(alg_sd_wtrtt[alg][sd]-0*alg_sd_wtrtt[BASE][sd]) for sd in sorted_sd]

    setupMPPDefaults()
    colors = CommonConf.getLineColorsDict()
    fmts = CommonConf.getLineFormatsDict()
    linewidth = CommonConf.getLineMarkersLWDict()
    mrkrs = CommonConf.getLineMarkersDict()
    mrkrsize = CommonConf.getLineMarkersSizeDict()
    for solver in algs:
        fig = pp.figure(figsize=(12,6))
        ax = fig.add_subplot(111)

        #if solver == BASE:
        #    continue
        ys = alg_sorted_lats[solver]
        print solver
        if solver == 'ecmp' or solver == 'edksp':
            markeredgecolor = colors[solver]
        else:
            markeredgecolor = 'None'
        ax.plot(
            #alg_sorted_lats[BASE],
            ys,
            alpha=0.5,
            color=colors[solver],
            label=CommonConf.gen_label(solver),
            linewidth=linewidth[solver],
            linestyle='None',
            markevery=1,
            markeredgecolor=markeredgecolor,
            markeredgewidth=mrkrsize[solver]/4,
            marker=mrkrs[solver],
            markersize=mrkrsize[solver]*1.5)

        ax.set_xlabel("Node Pairs")
        if rtt_hop == 'rtt':
            ax.set_ylabel("RTT (ms)")
        else:
            ax.set_ylabel("hop count")

        ax.spines['right'].set_visible(False)
        ax.spines['top'].set_visible(False)
        ax.yaxis.set_ticks_position('left')
        ax.xaxis.set_ticks_position('bottom')

        ax.xaxis.set_ticks([])
        # handles, labels = ax.get_legend_handles_labels()
        # # or sort them by labels
        # hl = sorted(zip(handles, labels), key=operator.itemgetter(1))
        # hl = hl[1:5]+[hl[0]]+[hl[5]] # Put racke in correct position
        # handles2, labels2 = zip(*hl)
        # ax.legend(handles2, labels2, loc='best', borderaxespad=0., fancybox=True, ncol=3)
        pp.locator_params(nbins=4)
        pp.tight_layout(pad=0)
        pp.savefig(out_file.split('.')[0]+solver+'.pdf')



if __name__ == "__main__":
    if len(sys.argv) < 5:
        print "Usage: " + sys.argv[0] + " rtt_file paths_dir out_file rtt/hop"
    rtt_file = sys.argv[1]
    paths_dir = sys.argv[2]
    out_file = sys.argv[3]
    rtt_hop = sys.argv[4]
    plot_latency(rtt_file, paths_dir, out_file, rtt_hop)
