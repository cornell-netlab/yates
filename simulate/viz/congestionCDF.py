from __future__ import division
import CommonConf
from collections import OrderedDict
import numpy as np
import sys
import matplotlib.pyplot as pp

X_LABEL = "Congestion"
Y_LABEL = "CDF (fraction of edges)"

def display (all_congestions, directory):
  sort_cong = OrderedDict()
  for scheme in sorted(all_congestions.keys()):
    sort_cong[scheme] = get_scheme_congestions(all_congestions, scheme)

  CommonConf.setupMPPDefaults()
  colors = CommonConf.getLineColorsDict()
  fmts = CommonConf.getLineFormatsDict()
  linewidth = CommonConf.getLineMarkersLWDict()
  mrkrs = CommonConf.getLineMarkersDict()
  mrkrsize = CommonConf.getLineMarkersSizeDict()

  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  mxl = 0
  num_sample=20
  for solver, cong_dist in sort_cong.iteritems():
    ys = [x/len(cong_dist) for x in range(0, len(cong_dist))]
    gap=int(len(ys)/num_sample)+1
    xx=[]
    yy=[]
    for i in range(0,len(ys)):
      if (i%gap==0):
        xx.append(cong_dist[i])
        yy.append(ys[i])
    if (len(ys)-1)%gap!=0:
      xx.append(cong_dist[-1])
      yy.append(ys[-1])

    ydevs = [0] * len(yy)
    ax.errorbar(xx, yy, yerr=ydevs,
            alpha=0.8,
            color=colors[solver],
            label=CommonConf.gen_label(solver),
            linewidth=linewidth[solver],
            linestyle=fmts[solver],
            marker=mrkrs[solver],
            markersize=mrkrsize[solver])

  ax.set_xlabel(X_LABEL)
  ax.set_ylabel(Y_LABEL)
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  pp.ylim(0.2,1.01)
  pp.savefig(directory+"/CongestionCDF.svg")


for scheme in []:
  get_sort_cong
  plot( get_sort_cong, range(0, len(get_sort_cong))/len(get_sort_cong),
      color=color[scheme])


def parse_congestions_file (filename):
    all_congestions = OrderedDict()
    scheme = ""
    iteration = ""
    links = dict()
    with open(filename) as f:
        for line in f.readlines():
            if line[0] == "#":
                continue
            if "host" in line:
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

def get_scheme_congestions (all_congestions, scheme):
    scm_congestion = all_congestions[scheme]
    congestions = []
    for iteration, congs in scm_congestion.iteritems():
        for l,c in congs.iteritems():
          congestions.append(c)
    sort_cong = sorted(congestions)
    return sort_cong

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + " <edge_congestion_file>"
    else:
        directory='/'.join(sys.argv[1].split('/')[:-1])
        all_congestions = parse_congestions_file(sys.argv[1])
        display(all_congestions, directory)
