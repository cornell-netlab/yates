import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import matplotlib.pyplot as pp

EXPERIMENT_NAME = "CongestionVsIterations"
X_LABEL         = "Iterations"
Y_LABEL         = "Congestion"
            
def main(dirn, fname): 
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname)
     
  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormats()
  mrkrs = CommonConf.getLineMarkers()
  fig = pp.figure()
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) : 
    ax.errorbar(xs, ys, yerr=ydevs, label=solver, marker=mrkrs[index], linestyle=fmts[index])
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(loc='best', fancybox=True)

  pp.savefig(dirn+"/"+fname+".pdf")
  pp.show()

if __name__ == "__main__":
  main("expData", EXPERIMENT_NAME)

