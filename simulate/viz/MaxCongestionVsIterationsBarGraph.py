import CommonConf
import CommonViz
import random

import re
from collections import OrderedDict
import numpy as np
import matplotlib.pyplot as pp
import sys

EXPERIMENT_NAME = "MaxCongestionVsIterations"
X_LABEL         = "Iterations"
Y_LABEL         = "Congestion"


def main(dirn, fnamei, solvers):
    def getrandomColor():
        curc= "#%06x" % random.randint(0, 0xFFFFFF);
        return curc
    (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(dirn, fnamei, solvers)

    pp.rcParams['font.size'] = 13
    pp.rcParams['ytick.labelsize'] = 15
    pp.rcParams['xtick.labelsize'] = 15
    pp.rcParams['legend.fontsize'] = 8
    pp.rcParams['lines.markersize'] = 8
    pp.rcParams['axes.titlesize'] = 18
    pp.rcParams['axes.labelsize'] = 15

    fig=pp.figure()
    ax=fig.add_subplot(111)

    #totT=len(xs);
    totT=100;

    subjects=np.arange(totT).tolist();

    totP=len(ysPerSolver);

    x=np.arange(len(subjects));

    colorlist=[];
    for i in range (totP):
        colorlist.append(getrandomColor())

    y=[];
    solverlabel=[]
    for (solver, ys), (solver, ydevs) in zip(ysPerSolver.iteritems(),ydevsPerSolver.iteritems()) :
        y.append(ys[0:totT]);
        solverlabel.append(solver);
    width=0.9/(1+totP);

    print y
    print x
    allbar=[];

    for i ,yi in enumerate(y):
        barlist=pp.bar(x+i*width, yi, width)
        for j in range(0, totT):
            barlist[j].set_color(colorlist[i])
        allbar.append(barlist[0]);

    ax.legend(allbar, solverlabel)

    #pp.xticks(x+totP/2.*width, subjects)
    ax.set_xlabel(X_LABEL);
    ax.set_ylabel(Y_LABEL);


    pp.savefig(dirn+"/"+fname+".pdf")
    pp.show()

if __name__ == "__main__":
    main("expData", EXPERIMENT_NAME, set(sys.argv[1:]))

