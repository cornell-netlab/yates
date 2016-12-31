import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import random
import sys
import matplotlib.pyplot as pp
import pylab


translate = { 'tput' : 'Throughput',
        'closs' : 'Congestion Loss',
        'floss' : 'Failure Loss',
        'maxc' : 'Max Congestion',
        'emaxc' : 'Scheduled Link Load',
        'meanc' : 'Mean Congestion',
        'medianc' : 'Median Congestion',
        'stime' : 'Solver Time',
        }

metrics = ['tput', 'closs', 'maxc', 'meanc', 'floss']


def main(dirn, solvers):
  xs, ysPerSolver, ydevsPerSolver = dict(), dict(), dict()
  (xs['tput'], ysPerSolver['tput'], ydevsPerSolver['tput']) = CommonViz.parseData(dirn, "TotalThroughputVsIterations", solvers)
  (xs['maxc'], ysPerSolver['maxc'], ydevsPerSolver['maxc']) = CommonViz.parseData(dirn, "MaxCongestionVsIterations", solvers)
  (xs['emaxc'], ysPerSolver['emaxc'], ydevsPerSolver['emaxc']) = CommonViz.parseData(dirn, "MaxExpCongestionVsIterations", solvers)
  (xs['meanc'], ysPerSolver['meanc'], ydevsPerSolver['meanc']) = CommonViz.parseData(dirn, "MeanCongestionVsIterations", solvers)
  (xs['medianc'], ysPerSolver['medianc'], ydevsPerSolver['medianc']) = CommonViz.parseData(dirn, "k70CongestionVsIterations", solvers)
  (xs['closs'], ysPerSolver['closs'], ydevsPerSolver['closs']) = CommonViz.parseData(dirn, "CongestionLossVsIterations", solvers)
  (xs['floss'], ysPerSolver['floss'], ydevsPerSolver['floss']) = CommonViz.parseData(dirn, "FailureLossVsIterations", solvers)
  (xs['stime'], ysPerSolver['stime'], ydevsPerSolver['stime']) = CommonViz.parseData(dirn, "TimeVsIterations", solvers)

  #82 262    * 262/180
  # median = 82 + (262 - 82)/2   |    262  = 65th percentile
  for solver,ys in ysPerSolver['meanc'].iteritems():
      ysPerSolver['meanc'][solver] = [y * 262/180 for y in ys]

  CommonConf.setupMPPDefaults()
  colors = {"tput":"black", "closs":"red", "floss":"blue", 'maxc':'orange', 'meanc' :
          'grey', 'medianc':'magenta'}

  fig, axes = pp.subplots(1, len(solvers), sharex=True, sharey=True, figsize=(20,12))

  first = True
  for ax,solver in zip(axes, solvers):
      for metric in metrics:
          ys = ysPerSolver[metric][solver]
          xs_arr = np.asarray(xs[metric])
          ax.plot(xs_arr, ys,
                    alpha=0.8,
                    color=colors[metric],
                    label=translate[metric],
                    linewidth=4,
                    linestyle="-",
                    marker=None)
      spines_to_remove = ['top', 'right']
      for spine in spines_to_remove:
              ax.spines[spine].set_visible(False)
      ax.xaxis.set_ticks_position('none')
      ax.yaxis.set_ticks_position('none')
      if not first:
          ax.spines['left'].set_alpha(0.2)
      if first:
          ax.legend(loc="upper left", borderaxespad=-2., fancybox=True,
                  ncol=len(metrics))
      first = False

  pp.ylim(-0.01,1.02)
  pp.subplots_adjust(wspace=0, hspace=0)
  pp.savefig(dirn+"/timeline.pdf")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " RunId" + " [optional (list_of_schemes)]"
  else:
        main(sys.argv[1], ["optimalmcf", "ecmp", "semimcfksp", "ffced",
            "semimcfmcfftenv", "raeke", "semimcfraeke"])
