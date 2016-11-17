import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import random
import sys
import matplotlib.pyplot as pp
import pylab

EXPERIMENT_NAME = "TotalThroughputVsIterations"
X_LABEL         = "TM"
Y_LABEL         = "Throughput (fraction of total demand)"

random.seed()

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

  index = 0
  #xs.append(max(xs)+2)
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/10
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
  ax.legend(loc=0, borderaxespad=1., fancybox=True)
  ymin, ymax = pp.ylim()
  pp.ylim(0.5,1.05)

  xa = ax.get_xaxis()
  xa.set_major_locator(pylab.MaxNLocator(integer=True))
  #ax.annotate('link (s2-s12) fails', xy=(1.8, 0.54), xytext=(0.95, 0.6),
  #          arrowprops=dict(facecolor='black', shrink=0.05), fontsize=14
  #          )
  #pp.axvline(1.8, linestyle='dashed' )
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)

  #pp.savefig(dirn+"/"+fname+"-".join(solvers)+".pdf")
  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".svg")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " RunId" + " [optional (list_of_schemes)]"
  else:
      if len(sys.argv) > 2:
        if sys.argv[2] == "Sink":
            EXPERIMENT_NAME = "TotalSinkThroughputVsIterations"
            main("expData/"+sys.argv[1], EXPERIMENT_NAME, set(sys.argv[3:]))
        else:
            main("expData/"+sys.argv[1], EXPERIMENT_NAME, set(sys.argv[2:]))
      else:
        main("expData/"+sys.argv[1], EXPERIMENT_NAME, set(sys.argv[2:]))
