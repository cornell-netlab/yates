import CommonConf

from collections import OrderedDict
import numpy as np
import sys
import matplotlib.pyplot as pp
from scipy.interpolate import pchip

X_LABEL = "RTT (ms)"
Y_LABEL = "Fraction of traffic delivered"
solver_list = ['ecmp', 'optimalmcf', 'semimcfraeke', 'raeke', 'semimcfmcfftenv', 'ffced','semimcfksp']
#solver_list = ['ecmp', 'optimalmcf', 'edksp', 'raeke', 'ksp', 'vlb']
def display (all_latencies, directory): # all_latencies : solver -> #tm(0-23) -> path_len -> frac_traffic
    scheme_latency_dist = OrderedDict()  # solver -> path_length -> (frac_tput_mean, frac_tput_std)
    for solver in all_latencies.keys():
        lat_percentile = get_latency_percentile(all_latencies, solver)
        scheme_latency_dist[solver] = lat_percentile
    CommonConf.setupMPPDefaults()
    fmts = CommonConf.getLineFormatsDict()
    mrkrs = CommonConf.getLineMarkersDict()
    mrkrsize = CommonConf.getLineMarkersSizeDict()
    mrkrlw = CommonConf.getLineMarkersLWDict()
    colors = CommonConf.getLineColorsDict()
    fig = pp.figure(figsize=(9,6))
    ax = fig.add_subplot(111)
    max_lat = 0
    for solver, latencies in scheme_latency_dist.iteritems():
        max_lat = max(max_lat, max(latencies.keys()))

    gap=1
    for solver, latencies in scheme_latency_dist.iteritems():
        if solver not in solver_list:
            continue
        xs = []
        xsall = sorted(latencies.keys())
        for i in range(0,len(xsall)):
          if (i%gap==0):
            xs.append(xsall[i])
        if (len(xsall)-1)%gap!=0:
          xs.append(xsall[-1])

        ys = [latencies[lat][0] for lat in xs]
        ydevs = [latencies[lat][1] for lat in xs]
#        ax.plot((xs[-1], max_lat), (ys[-1], ys[-1]), linestyle=':',
#                marker=mrkrs[solver],
#                color=colors[solver],
#                markersize=mrkrsize[solver],
#                markerfacecolor='none',
#                markeredgecolor=colors[solver],
#                markeredgewidth=mrkrsize[solver]/4,
#                linewidth=mrkrlw[solver])

        new_xs = np.linspace(min(xs), max(xs), (max(xs)-min(xs))*3+1)
        print new_xs
        yinterp = pchip(xs, ys)
        ydevsinterp = pchip(xs, ydevs)
        new_ys = yinterp(new_xs)
        new_ydevs = ydevsinterp(new_xs)
        ax.errorbar(new_xs, new_ys,# yerr=new_ydevs,
                label=CommonConf.gen_label(solver),# marker=mrkrs[solver],
                linestyle=fmts[solver],
                color=colors[solver],
                markersize=mrkrsize[solver],
                markerfacecolor='none',
                markeredgecolor=colors[solver],
                markeredgewidth=mrkrsize[solver]/4,
                markevery=None,# len(new_xs)/20,
                errorevery=len(new_xs)/20,
                linewidth=mrkrlw[solver])
        ax.fill_between(new_xs, new_ys-new_ydevs, new_ys+new_ydevs,
                        facecolor=colors[solver], alpha=0.3, interpolate=True,
                        edgecolor=colors[solver])
    ax.set_xlabel(X_LABEL)
    ax.set_ylabel(Y_LABEL)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.yaxis.set_ticks_position('left')
    ax.xaxis.set_ticks_position('bottom')
    ax.legend(loc='best', borderaxespad=0., fancybox=True)
    pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
    pp.ylim(0.0,1.05)
    xmin,xmax = ax.get_xlim()
    xmax = (xmax + (xmax%2))
    pp.xlim(3, 350)
    pp.tight_layout(pad=0)
    pp.savefig(directory+"/LatencyCDF.pdf")


def parse_latency_file (filename):
    all_latencies = OrderedDict() # solver -> #tm(0-23) -> path_len -> frac_traffic
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

def get_latency_percentile (all_latencies, scheme): # all_latencies : solver -> #tm(0-23) -> path_len -> frac_traffic
    scm_latency = all_latencies[scheme]     # #tm(0-23) -> path_len -> frac_traffic
    latency_percentiles = dict()
    latency_mean_percentile = dict()
    latency_val_set = set() # set of path_lengths
    for latencies in scm_latency.values():
        latency_val_set.update(latencies.keys())
    latency_values = sorted(latency_val_set) # sorted list of path_lengths
    for iteration, latencies in scm_latency.iteritems():
        prev_percentile = 0
        for lat in latency_values:
            percentiles = latency_percentiles.get(lat, [])
            percentile = latencies.get(lat, prev_percentile)
            percentiles.append(percentile)
            latency_percentiles[lat] = percentiles
            prev_percentile = percentile
    #for l in latency_values:
    #    print l
    #    print latency_percentiles.get(l, [])
    for lat,pers in latency_percentiles.iteritems():
        latency_mean_percentile[lat] = (np.mean(pers), np.std(pers))
    return latency_mean_percentile # path_length -> (frac_tput_mean, frac_tput_std)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + " <latency_file>"
    else:
        directory='/'.join(sys.argv[1].split('/')[:-1])
        all_latencies = parse_latency_file(sys.argv[1])
        display(all_latencies, directory)
