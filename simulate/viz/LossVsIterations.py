import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import matplotlib.pyplot as pp
import random
import sys

EXPERIMENT_NAME = "LossVsIterations"
X_LABEL         = "Iterations"
Y_LABEL         = "Loss"

random.seed(5)

def main(dirn, fname):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, set())

  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormats()
  mrkrs = CommonConf.getLineMarkers()
  colors = CommonConf.getLineColors()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    ax.errorbar(xs_arr, ys, yerr=ydevs, label=solver, marker=mrkrs[index],
        linestyle=fmts[index], color=colors[index])
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  pp.savefig(dirn+"/"+fname+".svg")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " topology_name" + " <Failure/Congestion>"
  else:
    main("expData/"+sys.argv[1], sys.argv[2]+EXPERIMENT_NAME)
