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
X_LABEL         = "Interval (hours)"
Y_LABEL         = "Time (sec.)"
NUMTM=91
def main(dirn, fname, solvers):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fname, solvers)

  CommonConf.setupMPPDefaults()
  colors = CommonConf.getLineColorsDict()
  fmts = CommonConf.getLineFormatsDict()
  linewidth = CommonConf.getLineMarkersLWDict()
  mrkrs = CommonConf.getLineMarkersDict()
  mrkrsize = CommonConf.getLineMarkersSizeDict()

  fig = pp.figure(figsize=(8,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  #xs.append(max(xs)+2)
  for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
    if solver not in CommonConf.solver_list:
        continue
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    xs_arr[-1]+= random.random()*2
    avg_y,std_y = np.mean(np.asarray(ys)), np.std(np.asarray(ys))
    #ys.append(avg_y)
    #ydevs.append(std_y)

    ax.errorbar(xs_arr[1:NUMTM], ys[1:NUMTM], yerr=ydevs[1:NUMTM],
            alpha=0.8,
            color=colors[solver],
            label=CommonConf.gen_label(solver),
            linewidth=linewidth[solver],
            linestyle=fmts[solver],
            marker=mrkrs[solver],
            markersize=mrkrsize[solver])

  ax.spines['top'].set_visible(False)
  ax.spines['right'].set_visible(False)
  ax.xaxis.set_ticks_position('none')
  ax.yaxis.set_ticks_position('none')
  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  #ax.set_yscale("log", nonposy='clip')
  ax.legend(loc='best', borderaxespad=0., fancybox=True, ncol=3)
  pp.subplots_adjust(left=0.12, right=0.95, top=0.9, bottom=0.12)
  pp.xlim(0,NUMTM)
  pp.savefig(dirn+"/"+fname+"-".join(solvers)+".pdf")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " RunId" + " [optional (list_of_schemes)]"
  else:
    main(sys.argv[1], EXPERIMENT_NAME, set(sys.argv[2:]))
