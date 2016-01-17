import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import matplotlib.pyplot as pp
import random
import sys
random.seed(5)
EXPERIMENT_NAME = "CongestionVsIterations"
X_LABEL         = "Iterations"
Y_LABEL         = "Congestion"

def main(dirn, fname, solvers):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, solvers)

  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormats()
  mrkrs = CommonConf.getLineMarkers()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    ax.errorbar(xs_arr, ys, yerr=ydevs, label=solver, marker=mrkrs[index], linestyle=fmts[index], alpha=0.8)
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)

  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".svg")

if __name__ == "__main__":
  if len(sys.argv) < 3:
    print "Usage: " + sys.argv[0] + " RunId" +" [Max | Mean | percentile (k10, k20, ... , k95]" + "[optional (list_of_schemes)]"
  else:
    main("expData/"+sys.argv[1], sys.argv[2]+EXPERIMENT_NAME, set(sys.argv[3:]))
