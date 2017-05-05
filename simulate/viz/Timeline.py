import CommonConf
import CommonViz

import re
from collections import OrderedDict
import numpy as np
import random
import sys
import matplotlib.pyplot as pp
import pylab

NUMTM=91

translate = { 'tput' : 'Throughput',
        'closs' : 'Congestion Loss',
        'floss' : 'Failure Loss',
        'maxc' : 'Max Congestion',
        'emaxc' : 'Scheduled Link Load',
        'meanc' : 'Mean Congestion',
        'medianc' : 'Median Congestion',
        'stime' : 'Solver Time',
        }

#metrics = ['tput', 'closs', 'maxc', 'meanc', 'floss']
#metrics = ['tput', 'floss', 'closs', 'maxc']
metrics = ['tput', 'closs', 'maxc']

colors = {"tput":"black", "closs":"red", "floss":'blue', 'maxc':'green', 'meanc' :
          'grey', 'medianc':'magenta'}


def setupMPPDefaults():
    pp.rcParams['font.size'] = 50
    pp.rcParams['mathtext.default'] = 'regular'
    pp.rcParams['ytick.labelsize'] = 50
    pp.rcParams['xtick.labelsize'] = 50
    pp.rcParams['legend.fontsize'] = 16
    pp.rcParams['lines.markersize'] = 12
    pp.rcParams['axes.titlesize'] = 40
    pp.rcParams['axes.labelsize'] = 50
    pp.rcParams['axes.edgecolor'] = 'grey'
    pp.rcParams['axes.linewidth'] = 3.0
    pp.rcParams['axes.grid'] = True
    pp.rcParams['grid.alpha'] = 0.4
    pp.rcParams['grid.color'] = 'grey'
    pp.rcParams['legend.frameon'] = True
    pp.rcParams['legend.framealpha'] = 0.4
    pp.rcParams['legend.numpoints'] = 1
    pp.rcParams['legend.scatterpoints'] = 1

def create_legend(dirn):
    fig = pp.figure()
    figlegend = pp.figure(figsize=(14,0.6))
    ax = fig.add_subplot(111)
    handles = []
    props = dict(alpha=0.6, edgecolors='none', marker='|')
    for metric in metrics:
        handles.append(ax.scatter([1], [1], c=colors[metric],
            linewidths=20,
            s=40, **props))

    figlegend.legend(handles, [translate[x] for x in metrics],loc=4, ncol=5)
    figlegend.savefig(dirn+'/metriclegend.pdf')


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

  print np.mean(np.asarray(ysPerSolver['maxc']['semimcfraeke'])/np.asarray(ysPerSolver['maxc']['optimalmcf']))
  print np.min(np.asarray(ysPerSolver['maxc']['semimcfraeke'])/np.asarray(ysPerSolver['maxc']['optimalmcf']))
  print np.max(np.asarray(ysPerSolver['maxc']['semimcfraeke'])/np.asarray(ysPerSolver['maxc']['optimalmcf']))
  print np.std(np.asarray(ysPerSolver['maxc']['semimcfraeke'])/np.asarray(ysPerSolver['maxc']['optimalmcf']))
  print np.mean(ysPerSolver['maxc']['semimcfraeke'])/np.mean(ysPerSolver['maxc']['optimalmcf'])
  setupMPPDefaults()
  fig, axes = pp.subplots(1, len(solvers), sharex=True, sharey=True,
                          figsize=(24,12))

  first = True
  for ax,solver in zip(axes, solvers):
      for metric in metrics:
          ys = ysPerSolver[metric][solver][:NUMTM]
          xs_arr = np.asarray(xs[metric][:NUMTM])
          ydev = np.std(ys)
          linewidth = 5
          if ydev < 1e-2:
              linewidth = linewidth*2
          ax.plot(xs_arr, ys,
                    alpha=0.8,
                    color=colors[metric],
                    label=translate[metric],
                    linewidth=linewidth,
                    linestyle="-",
                    marker=None,
                  zorder = 1/(np.mean(ys)+1))
      spines_to_remove = ['top', 'right']
      for spine in spines_to_remove:
              ax.spines[spine].set_visible(False)
      ax.xaxis.set_ticks_position('none')
      ax.yaxis.set_ticks_position('none')
      #xmin, xmax = ax.get_xlim()
      #ax.xaxis.set_ticks(np.arange(xmin, xmax, 50))
      ax.tick_params(labelbottom='off')
      if not first:
          ax.spines['left'].set_alpha(0.2)
      #if first:
      #    ax.legend(loc="lower left", borderaxespad=-4., fancybox=True,
      #            ncol=len(metrics))
      first = False
      ax.title.set_text(CommonConf.gen_label(solver))
      ax.title.set_rotation(0)
      ax.title.set_va('bottom')

  fig.text(0.5, 0.04, 'Time', ha='center')
  fig.text(0.03, 0.5, 'Metric', va='center', rotation='vertical')
  pp.ylim(-0.01,1.02)
  pp.subplots_adjust(wspace=0, hspace=0)
  pp.savefig(dirn+"/timeline.pdf")

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " RunId" + " [optional (list_of_schemes)]"
  else:
        main(sys.argv[1], ["optimalmcf", "ecmp", "semimcfksp", "ffced",
            "semimcfmcfftenv", "raeke", "semimcfraeke"])
  create_legend(sys.argv[1])
