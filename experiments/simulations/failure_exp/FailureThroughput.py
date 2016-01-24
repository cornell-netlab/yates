import sys
sys.path.append("../../../simulate/viz/")
import CommonConf
import CommonViz

import re
from os import listdir
from collections import OrderedDict
import numpy as np
import random
import matplotlib.pyplot as pp
import os.path as op

X_LABEL         = "Scale"
max_scale       = 15

random.seed()

def getscale(st):
    position=st.find('_scale_')+7
    return st[position:st.find('_',position)]

def include(alg):
    if 'raekeft' in alg:
        return False
    if 'kspft' in alg:
        return False
    return True

def main(dirn, fname, Y_LABEL):
  folders=[f  for f in listdir(dirn) if op.isdir(op.join(dirn, f)) & (f.startswith('fail'))];
  mergedTable={}
  for folder in folders:
      cur_scale= int(getscale(folder))
      (_, ysPerSolver, _) = CommonViz.parseData(dirn+folder, fname, set())

      if not (cur_scale in mergedTable):
          mergedTable[cur_scale]={}
          for alg in ysPerSolver:
              if include(alg):
                  mergedTable[cur_scale][alg]=ysPerSolver[alg][2:]
      else:
          for alg in ysPerSolver:
              if include(alg):
                  mergedTable[cur_scale][alg] += ysPerSolver[alg][2:]
  xs=[]
  ys={}
  ydevs={}
  for scale in sorted(mergedTable):
      if scale > max_scale:
          continue;
      xs.append(scale)
      for alg in mergedTable[scale]:
          if alg in ys:
              ys[alg].append(np.mean(np.asarray(mergedTable[scale][alg])))
              ydevs[alg].append(np.std(np.asarray(mergedTable[scale][alg])))
          else:
              ys[alg]=[np.mean(np.asarray(mergedTable[scale][alg]))]
              ydevs[alg]=[np.std(np.asarray(mergedTable[scale][alg]))]


  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormats()
  mrkrs = CommonConf.getLineMarkers()
  colors = CommonConf.getLineColors()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0

  print('maxxs=',max(xs))
  xs.append('Average')

  ymin=1
  for alg in ys:
      if min(ys[alg])<ymin:
          ymin=min(ys[alg])
  ymin=float(int((ymin-0.1)*10))/10
  y_offset=np.array([ymin]*len(xs))
  patterns=['','-', '+', 'x', '\\', '*', 'o', 'O', '.']*10
  for alg in sorted(ys):
    xs_arr = np.asarray(range(0,len(xs)))
    avg_y,std_y = np.mean(np.asarray(ys[alg])), np.std(np.asarray(ys[alg]))
    ys[alg].append(avg_y)
    ydevs[alg].append(std_y)
    ys_arr=np.asarray(ys[alg])-ymin
    #print('ys:',ys[alg])
    #print('ydevs:',ydevs[alg])
    #ax.errorbar(xs_arr, ys[alg], yerr=ydevs[alg], label=alg, marker=mrkrs[index], linestyle=fmts[index], color=colors[index])
    ax.bar(xs_arr+float(index)/len(ys)*0.9,ys_arr,yerr=ydevs[alg], width=[0.9/len(ys)]*len(ys_arr), bottom=y_offset, label=alg,align='center', color=colors[index], error_kw={'marker':mrkrs[index]})
    ax.set_xticks(xs_arr+0.35)
    ax.set_xticklabels(xs)
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  #ymin, ymax = pp.ylim()
  #pp.ylim(ymin,ymax+0.05)

  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  pp.savefig(dirn+"/"+fname+".svg")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage: ", sys.argv[0], " <congestion/throughput>"
        sys.exit(0)
    if sys.argv[1] == "congestion":
        EXPERIMENT_NAME="CongestionLossVsIterations"
        Y_LABEL         = "Congestion Loss (fraction of demand)"
    if sys.argv[1] == "failure":
        EXPERIMENT_NAME="FailureLossVsIterations"
        Y_LABEL         = "Failure  Loss (fraction of demand)"
    elif sys.argv[1] == "throughput":
        EXPERIMENT_NAME="TotalThroughputVsIterations"
        Y_LABEL         = "Throughput (fraction of demand)"
    main("/home/abilene/expData/", EXPERIMENT_NAME, Y_LABEL)
