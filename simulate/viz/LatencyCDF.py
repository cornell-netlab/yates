import CommonConf

from collections import OrderedDict
import matplotlib.pyplot as pp
import numpy as np
import sys

X_LABEL = "Latency"
Y_LABEL = "CDF"

def display (all_latencies, directory):
    scheme_latency_dist = OrderedDict()
    for solver in all_latencies.keys():
        lat_percentile = get_latency_percentile(all_latencies, solver)
        scheme_latency_dist[solver] = lat_percentile
    CommonConf.setupMPPDefaults()
    fmts = CommonConf.getLineFormats()
    mrkrs = CommonConf.getLineMarkers()
    colors = CommonConf.getLineColors()
    fig = pp.figure(figsize=(12,6))
    ax = fig.add_subplot(111)
    index = 0
    mxl = 0
    for solver, latencies in scheme_latency_dist.iteritems():
        xl = sorted(latencies.keys())[-1]
        if mxl < xl:
            mxl = xl

    for solver, latencies in scheme_latency_dist.iteritems():
        xs = sorted(latencies.keys())
        ys = [latencies[lat][0] for lat in xs]
        ydevs = [latencies[lat][1] for lat in xs]
        ax.plot((xs[-1], mxl), (ys[-1], ys[-1]), linestyle=fmts[index],
                color=colors[index])
        ax.errorbar(xs, ys, yerr=ydevs, label=solver, marker=mrkrs[index],
                linestyle=fmts[index], color=colors[index])
        index = index + 1
    ax.set_xlabel(X_LABEL)
    ax.set_ylabel(Y_LABEL)
    ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
    pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
    pp.ylim(ymax=1.0)
    pp.savefig(directory+"/LatencyCDF.svg")


def parse_latency_file (filename):
    all_latencies = OrderedDict()
    scheme = ""
    iteration = ""
    latency_dist = dict()
    with open(filename) as f:
        for line in f.readlines():
            if line[0] == "#":
                continue
            else:
                tokens = line.split('\t')
                if len(tokens) != 3:
                    if len(latency_dist) != 0:
                        all_latencies[scheme][int(iteration)] = latency_dist
                        latency_dist = dict()
                    continue
                if tokens[2] == '\n':
                    if len(latency_dist) != 0:
                        all_latencies[scheme][int(iteration)] = latency_dist
                        latency_dist = dict()
                    scheme = tokens[0]
                    iteration = tokens[1]
                    if scheme not in all_latencies.keys():
                        all_latencies[scheme] = dict()
                else:
                    latency_percentile = tokens[2].split(':')
                    latency_dist[float(latency_percentile[0].strip())] = float(latency_percentile[1].strip())
    return all_latencies

def get_latency_percentile (all_latencies, scheme):
    scm_latency = all_latencies[scheme]
    latency_percentiles = dict()
    latency_mean_percentile = dict()
    max_latency = 0
    for latencies in scm_latency.values():
        max_latency = max(max_latency, max(latencies.keys()))
    for iteration, latencies in scm_latency.iteritems():
        lat = 0
        prev_percentile = 0
        while lat <= max_latency:
            percentiles = latency_percentiles.get(lat, [])
            percentile = latencies.get(lat, prev_percentile)
            percentiles.append(percentile)
            latency_percentiles[lat] = percentiles
            prev_percentile = percentile
            lat += 1
        #for lat, percentile in latencies.iteritems():
       #     percentiles = latency_percentiles.get(lat, [])
       #     percentiles.append(percentile)
       #     latency_percentiles[lat] = percentiles
       # max_latency_for_iter = max(latencies)
       # l = max_latency_for_iter + 1
       # while l <= max_latency:
       #     percentiles = latency_percentiles.get(l, [])
       #     percentiles.append(latencies[max_latency_for_iter])
       #     latency_percentiles[l] = percentiles
       #     l += 1
    l = 1
    while l <= max_latency:
        print l
        print latency_percentiles.get(l, [])
        l += 1
    for lat,pers in latency_percentiles.iteritems():
        latency_mean_percentile[lat] = (np.mean(pers), np.std(pers))
    return latency_mean_percentile


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + " <latency_file>"
    else:
        directory='/'.join(sys.argv[1].split('/')[:-1])
        all_latencies = parse_latency_file(sys.argv[1])
        display(all_latencies, directory)
