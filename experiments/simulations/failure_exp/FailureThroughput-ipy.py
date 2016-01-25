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
scales_to_consider       = [1, 1.5, 2]

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

def main(dirn, fname, Y_LABEL,scale_down_spf_tot=False, graph_type="bar",scale_up=False):
  folders=[f  for f in listdir(dirn) if op.isdir(op.join(dirn, f)) & (f.startswith('fail_'))];
  mergedTable={}
  for folder in folders:
      cur_scale = float(getscale(folder))
      if cur_scale not in scales_to_consider:
          continue
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

      if scale_down_spf_tot:
         spf_tot=mergedTable[cur_scale]['spf'][-1]
         #print(spf_tot)
         #print(folder,np.asarray(ysPerSolver['raeke'][2:])/spf_tot)
         for alg in ysPerSolver:
            for l in range(1,len(ysPerSolver[alg])-1): #1- len-1
              mergedTable[cur_scale][alg][-l]/= spf_tot
  xs=[]
  ys={}
  ydevs={}
  for scale in sorted(mergedTable.keys()):
      if scale not in scales_to_consider:
          continue;
      xs.append(scale)
      factor=scale if scale_up else 1.0
      for alg in mergedTable[scale]:
          if alg in ys:
              ys[alg].append(np.mean(np.asarray(mergedTable[scale][alg])*factor))
              ydevs[alg].append(np.std(np.asarray(mergedTable[scale][alg])*factor))
          else:
              ys[alg]=[np.mean(np.asarray(mergedTable[scale][alg])*factor)]
              ydevs[alg]=[np.std(np.asarray(mergedTable[scale][alg])*factor)]






  CommonConf.setupMPPDefaults()
  fmts = CommonConf.getLineFormats()
  mrkrs = CommonConf.getLineMarkers()
  colors = CommonConf.getLineColors()
  fig = pp.figure(figsize=(12,6))
  ax = fig.add_subplot(111)
  # ax.set_xscale("log", basex=2)

  index = 0

  print('maxxs=',max(xs))
  if (graph_type=="bar"):
    xs.append('Average')
  else:
    xs.append(xs[-1]+3)

  ymin=1
  for alg in ys:
      if min(ys[alg])<ymin:
          ymin=min(ys[alg])
  ymin=max(0.0, float(int((ymin-0.1)*10))/10)
  savey=ymin
  y_offset=np.array([ymin]*len(xs))
  patterns=['','-', '+', 'x', '\\', '*',  '.']*10
  for alg in sorted(ys):
    xs_arr = np.asarray(range(0,len(xs)))
    avg_y,std_y = np.mean(np.asarray(ys[alg])), np.std(np.asarray(ys[alg]))
    ys[alg].append(avg_y)
    ydevs[alg].append(0)
    ys_arr=np.asarray(ys[alg])-ymin
    #print('ys:',ys[alg])
    #print('ydevs:',ydevs[alg])
    #ax.errorbar(xs_arr, ys[alg], yerr=ydevs[alg], label=alg, marker=mrkrs[index], linestyle=fmts[index], color=colors[index])
    if graph_type == "bar":
      ax.bar(xs_arr+float(index)/len(ys)*0.9,ys_arr,yerr=ydevs[alg], width=[0.9/len(ys)]*len(ys_arr), bottom=y_offset, label=alg,
           align='center', color=colors[index])
      ax.set_xticks(xs_arr+0.35)
      ax.set_xticklabels(xs)
    elif graph_type == "line":
      ax.errorbar(xs, ys[alg], yerr=ydevs[alg], label=alg, marker=mrkrs[index], linestyle=fmts[index], color=colors[index])
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  ymin, ymax = pp.ylim()
  print ymin
  pp.ylim(savey, ymax+0.05)

  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  pp.savefig(dirn+"/failure_"+fname+".svg")




if __name__ == "__main__":
    main("/home/abilene/expData/", 'TotalThroughputVsIterations',"Throughput (fraction of demand)" ,scale_up=True)
    main("/home/abilene/expData/", 'TotalThroughputVsIterations',"Throughput (fraction of demand)" ,graph_type='line',scale_up=True)
    main("/home/abilene/expData/", 'TotalThroughputVsIterations',"Throughput (fraction of demand)" ,scale_up=False)

    main("/home/abilene/expData/", 'CongestionLossVsIterations',"Congestion Loss (fraction of demand)" ,scale_up=True)
    main("/home/abilene/expData/", 'FailureLossVsIterations',"Failure Loss (fraction of demand)" ,scale_up=True)
    main("/home/abilene/expData/", 'TMChurnVsIterations',"TM Churn" )
    main("/home/abilene/expData/", 'RecoveryChurnVsIterations',"Recovery Churn" )
