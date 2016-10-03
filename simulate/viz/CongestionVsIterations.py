import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import random
import sys
import matplotlib.pyplot as pp
random.seed(5)
EXPERIMENT_NAME = "CongestionVsIterations"
X_LABEL         = "TM"
Y_LABEL         = "Congestion"

def main(dirn, fname, solvers):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, solvers)

  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormatsDict()
  mrkrs = CommonConf.getLineMarkersDict()
  linewidth = CommonConf.getLineMarkersLWDict()
  mrkrsize = CommonConf.getLineMarkersSizeDict()
  colors = CommonConf.getLineColorsDict()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)
  xs.append(max(xs)+2)
  index = 0
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    xs_arr[-1]+= random.random()*2
    avg_y,std_y = np.mean(np.asarray(ys)), np.std(np.asarray(ys))
    ys.append(avg_y)
    ydevs.append(std_y)


    ax.errorbar(xs_arr, ys, yerr=ydevs, label=CommonConf.gen_label(solver), marker=mrkrs[solver],
        markersize=mrkrsize[solver], linewidth=linewidth[solver],
        linestyle=fmts[solver], alpha=0.8, color=colors[solver])
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  ymin, ymax = pp.ylim()
  pp.ylim(ymin-0.1,ymax+0.1)
  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".svg")

if __name__ == "__main__":
  if len(sys.argv) < 3:
    print "Usage: " + sys.argv[0] + " RunId" +" [Max | Mean | percentile (k10, k20, ... , k95]" + "[optional (list_of_schemes)]"
  else:
    main("expData/"+sys.argv[1], sys.argv[2]+EXPERIMENT_NAME, set(sys.argv[3:]))
