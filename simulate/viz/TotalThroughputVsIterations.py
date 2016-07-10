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

def fix_name(alg):
    return alg.replace('raeke', 'raecke')

def main(dirn, fname, solvers):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, solvers)

  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormatsDict()
  mrkrs = CommonConf.getLineMarkersDict()
  colors = CommonConf.getLineColorsDict()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0
  #xs.append(max(xs)+2)
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/4-0.1
    #xs_arr[-1]+= random.random()*4
    #avg_y,std_y = np.mean(np.asarray(ys)), np.std(np.asarray(ys))
    #ys.append(avg_y)
    #ydevs.append(std_y)
    ax.errorbar(xs_arr, ys, yerr=ydevs, label=fix_name(solver), marker=mrkrs[solver],
        linestyle=fmts[solver], alpha=0.8, color=colors[solver], markersize=12)
    index = index + 1

  ax.set_xlabel(X_LABEL, fontsize=14);
  ax.set_ylabel(Y_LABEL, fontsize=14);
  ax.legend( loc=0, borderaxespad=1., fancybox=True, fontsize=15)
  ymin, ymax = pp.ylim()
  pp.ylim(0.5,1.05)
  pp.xlim(-0.2,3.2)

  xa = ax.get_xaxis()
  xa.set_major_locator(pylab.MaxNLocator(integer=True))
  ax.annotate('link (s2-s12) fails', xy=(1.8, 0.54), xytext=(0.95, 0.6),
            arrowprops=dict(facecolor='black', shrink=0.05), fontsize=14
            )
  pp.axvline(1.8, linestyle='dashed' )
  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)

  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".pdf")

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
