import CommonConf
import CommonViz

import matplotlib.pyplot as plt
import numpy as np
from os import listdir
import os.path as op
from itertools import cycle
from matplotlib.lines import Line2D
import matplotlib.backends.backend_pdf
import math
import random


def plotfigure(metric_name,dotfile):
    curf="/home/praveenk/tmp/expData";
    todofolder=[f for f in listdir(curf) if op.isdir(op.join(curf, f)) & (dotfile in f) & (f[-1]=='0')]
    mydict={}
    curname=0;
    totiter=0
    for folder in todofolder:
        curname += 1
        picked_file=[f for f in listdir(op.join(curf,folder)) if op.isfile(op.join(curf,folder, f)) & (metric_name+'.dat' in f) & (not 'swp' in f)]
        if (len(picked_file)>1):
            assert(False)
        picked_file=picked_file[0]
        f=open(op.join(curf,folder,picked_file))
        for line in f.readlines():
            if "solver" in line:
                continue
            words=line.split("\t")
            if (len(words)<4):
                continue
            if (not words[0] in mydict):
                mydict[words[0]]=[[] for i in range(len(todofolder))]
            if (int(words[1])>totiter):
                totiter=int(words[1])
            mydict[words[0]][curname-1].append(float(words[2]))
    import numpy
    mean_list={}
    var_list={}
    for alg in mydict:
        mean_list[alg]=[]
        var_list[alg]=[]
        for all_num in mydict[alg]:
            mean_list[alg].append(numpy.mean(numpy.array(all_num), axis=0))
            var_list[alg].append(numpy.std(numpy.array(all_num), axis=0))

    totiter+= 1
    fig=plt.figure(figsize=(18,9))
    dotted=0
    m=CommonConf.getLineMarkers()
    linest=CommonConf.getLineFormats()
    colors=CommonConf.getLineColors()

    pdf = matplotlib.backends.backend_pdf.PdfPages(dotfile+metric_name+".pdf")

    for alg in sorted(mydict):
        xs=np.asarray([i*10 for i in range(0,len(mean_list[alg]))])+random.random()*5
        plt.errorbar(xs,mean_list[alg],var_list[alg],label=alg, linestyle=linest[dotted], marker=m[dotted],color=colors[dotted])
        dotted+=1
    x1,x2,y1,y2 = plt.axis()
    plt.axis((x1,x2+0.1,y1,y2+0.05))
    plt.legend(loc='best',prop={'size':14},bbox_to_anchor=(1.02, 1))
    plt.tight_layout(pad=20)
    plt.xlabel('random perturb %')
    plt.ylabel(metric_name)
    pdf.savefig(plt.gcf())
    plt.show()
    plt.close(fig)

    pdf.close()

plotfigure('MaxCongestionVsIterations','grid5h1EvenDemand')
plotfigure('TotalThroughputVsIterations','grid5h1EvenDemand')

