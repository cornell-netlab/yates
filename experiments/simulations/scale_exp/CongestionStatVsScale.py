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

EXPERIMENT_NAME = "EdgeExpCongestionVsIterations"
X_LABEL         = "Scale"
Y_LABEL         = "Fraction of saturated edges"

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

def getmaxfrac(li):
    tot=0
    for item in li:
        tot += 1 if item>0.0 else 0
    return float(tot)/len(li)

def get_avg_util(li):
    return sum(li)/len(li)


def get_max_cong(li):
    return max(li)

def parse_congestion_file (filename):
    all_congestions= dict()
    scheme=""
    iteration =""
    links = []
    with open (filename) as f:
        for line in f.readlines():
            if line[0]=='#':
                continue
            else:
                if ':' in line:
                    if scheme=="":
                        print("ERROR!!!!!!!!!!!!!!!")
                    tokens =line.split(':')
                    all_congestions[scheme].append(float(tokens[1]))
                else:
                    tokens=line.split('\t')
                    if (len(tokens)<2):
                        continue
                    scheme=tokens[0]
                    all_congestions[scheme]=[]
    for alg in all_congestions:
        all_congestions[alg]=get_avg_util(all_congestions[alg])
    for k,v in all_congestions.iteritems():
        print k, v
    return all_congestions

def main(dirn, fname):
  folders=[f  for f in listdir(dirn) if op.isdir(op.join(dirn, f)) & (f.startswith('scale'))];
  mergedTable={}
  for folder in folders:
      cur_scale= int(getscale(folder))
      print cur_scale
      ysPerSolver = parse_congestion_file(dirn+folder+"/"+fname+'.dat')
      if not (cur_scale in mergedTable):
          mergedTable[cur_scale]={}
          for alg,data in ysPerSolver.iteritems():
              if include(alg):
                  mergedTable[cur_scale][alg] = [data]
      else:
          for alg,data in ysPerSolver.iteritems():
              if include(alg):
                  mergedTable[cur_scale][alg] += [data]
  xs=[]
  ys={}
  ydevs={}
  lastone=50
  for scale in sorted(mergedTable):
      if scale>lastone:
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



  xs.append(max(xs)+3)
  for alg in sorted(ys):
    xs_arr = np.asarray(xs)
    xs_arr = xs_arr + random.random()/2
    xs_arr[-1]= max(xs)+3+float(index)/len(ys)*5
    avg_y,std_y = np.mean(np.asarray(ys[alg])), np.std(np.asarray(ys[alg]))
    ys[alg].append(avg_y)
    ydevs[alg].append(std_y)
    #print('ys:',ys[alg])
    #print('ydevs:',ydevs[alg])
    ax.errorbar(xs_arr, ys[alg], yerr=ydevs[alg], label=alg, marker=mrkrs[index],
        linestyle=fmts[index], color=colors[index])
    index = index + 1

  ax.set_xlabel(X_LABEL);
  ax.set_ylabel(Y_LABEL);
  ax.legend(bbox_to_anchor=(1., 1.), loc=2, borderaxespad=1., fancybox=True)
  ymin, ymax = pp.ylim()
  pp.ylim(-0.05,ymax+.05)

  pp.subplots_adjust(left=0.1, right=0.8, top=0.9, bottom=0.1)
  pp.savefig(dirn+"/"+fname+".svg")

if __name__ == "__main__":
    main("/home/abilene/expData/", EXPERIMENT_NAME)
