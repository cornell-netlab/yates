from __future__ import division
import CommonConf
from collections import OrderedDict
import numpy as np
import sys
import matplotlib.pyplot as pp
from matplotlib.ticker import ScalarFormatter
from matplotlib.patches import Rectangle

X_LABEL = "Congestion"
X_EXPLABEL = "Expected Load"
Y_LABEL = "Complementary CDF \n (fraction of edges)"
NUMTM = 91

def display (all_congestions, snapshot, directory, outfile):
  sort_cong = OrderedDict()
  for scheme in sorted(all_congestions.keys()):
    sort_cong[scheme] = get_scheme_congestions(all_congestions, snapshot, scheme)

  CommonConf.setupMPPDefaults()
  colors = CommonConf.getLineColorsDict()
  fmts = CommonConf.getLineFormatsDict()
  linewidth = CommonConf.getLineMarkersLWDict()
  mrkrs = CommonConf.getLineMarkersDict()
  mrkrsize = CommonConf.getLineMarkersSizeDict()

  fig = pp.figure(figsize=(9,6))
  ax = fig.add_subplot(111)
  mxl = 0
  num_sample=1000
  for solver, cong_dist in sort_cong.iteritems():
    if solver not in CommonConf.solver_list:
        continue
    ys = [x/len(cong_dist) for x in range(0, len(cong_dist))]
    gap=int(len(ys)/num_sample)+1
    xx=[]
    yy=[]
    for i in range(0,len(ys)):
      if (i%gap==0):
        xx.append(cong_dist[i])
        yy.append(1-ys[i])
    if (len(ys)-1)%gap!=0:
      xx.append(cong_dist[-1])
      yy.append(1-ys[-1])

    ydevs = [0] * len(yy)
    #ax.errorbar(xx, yy, yerr=ydevs,
    #        alpha=0.8,
    #        color=colors[solver],
    #        label=CommonConf.gen_label(solver),
    #        linewidth=linewidth[solver],
    #        linestyle=fmts[solver],
    #        marker=None,#mrkrs[solver],
    #        markersize=mrkrsize[solver])
    ax.plot(xx, yy,
            alpha=0.8,
            color=colors[solver],
            label=CommonConf.gen_label(solver),
            linewidth=linewidth[solver],
            linestyle=fmts[solver],
            marker=None,#mrkrs[solver],
            markersize=mrkrsize[solver])


  ax.set_xlabel(X_LABEL)
  ax.set_ylabel(Y_LABEL)
  ax.legend(loc='best', borderaxespad=0., fancybox=True)
  ax.spines['right'].set_visible(False)
  ax.spines['top'].set_visible(False)
  ax.xaxis.set_ticks_position('bottom')
  ax.yaxis.set_ticks_position('left')
  #pp.subplots_adjust(left=0.2, right=0.95, top=0.95, bottom=0.15)
  xmax = pp.xlim()[1]
  if xmax > 1:
      ax.set_xlabel(X_EXPLABEL)
      ax.set_xscale("log", nonposx='clip')
      ax.set_yscale("log", nonposx='clip')
      print pp.xlim()
      xmax = pp.xlim()[1]
      ax.xaxis.set_major_formatter(ScalarFormatter())
      #ax.yaxis.set_major_formatter(ScalarFormatter())
      ax.axvline(x=1, color='r', lw=2, ls='-')
      currentAxis = pp.gca()
      currentAxis.add_patch(Rectangle((1,0), xmax, 1, facecolor="red",
                                      alpha=0.05))
      pp.xlim(1e-2,1e2)
      pp.ylim(1e-4,1e0)
  pp.tight_layout(pad=0)
  pp.savefig(directory+"/"+outfile+".pdf")


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

def get_scheme_congestions (all_congestions, snapshot, scheme):
    scm_congestion = all_congestions[scheme]
    congestions = []
    for iteration, congs in scm_congestion.iteritems():
        if iteration > NUMTM:
            break
        for l,c in congs.iteritems():
          congestions.append(c)
        if snapshot:
            break
    sort_cong = sorted(congestions)
    return sort_cong

if __name__ == "__main__":
    print sys.argv
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + "<snapshot:yes/no> <edge_congestion_file> <outfile>"
    else:
        if sys.argv[1] == "yes":
            snapshot = True
        else:
            snapshot = False
        directory='/'.join(sys.argv[2].split('/')[:-1])
        outfile = sys.argv[3]
        all_congestions = parse_congestions_file(sys.argv[2])
        display(all_congestions, snapshot, directory, outfile)
