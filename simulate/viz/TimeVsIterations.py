import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import random
import sys
import matplotlib.pyplot as pp

random.seed(5)

EXPERIMENT_NAME = "TimeVsIterations"
X_LABEL         = "TM"
Y_LABEL         = "Time (sec.)"

def main(dirn, fname, solvers):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, solvers)

  CommonConf.setupMPPDefaults()
  colors = CommonConf.getLineColorsDict()
  fmts = CommonConf.getLineFormatsDict()
  linewidth = CommonConf.getLineMarkersLWDict()
  mrkrs = CommonConf.getLineMarkersDict()
  mrkrsize = CommonConf.getLineMarkersSizeDict()

  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  xs.append(max(xs)+2)
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    xs_arr[-1]+= random.random()*2
    avg_y,std_y = np.mean(np.asarray(ys)), np.std(np.asarray(ys))
    ys.append(avg_y)
    ydevs.append(std_y)

    ax.errorbar(xs_arr, ys, yerr=ydevs,
            alpha=0.8,
            color=colors[solver],
            label=CommonConf.gen_label(solver),
            linewidth=linewidth[solver],
            linestyle=fmts[solver],
            marker=mrkrs[solver],
            markersize=mrkrsize[solver])

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)

  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".svg")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " RunId" + " [optional (list_of_schemes)]"
  else:
    main("expData/"+sys.argv[1], EXPERIMENT_NAME, set(sys.argv[2:]))
