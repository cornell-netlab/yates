#!/usr/bin/env python
import operator
import sys
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as pp
import CommonConf
import numpy as np

solvers = ['raeke', 'ksp', 'ecmp', 'vlb', 'edksp', 'mcf']

def plot_pdf(pdfs, budget, out_file):
    CommonConf.setupMPPDefaults()
    fig, axes = pp.subplots(2, len(solvers)/2, sharex=True, sharey=True,
                          figsize=(12,6))
    colors=CommonConf.getLineColorsDict()
    fmts = CommonConf.getLineFormatsDict()
    linewidth = CommonConf.getLineMarkersLWDict()
    mrkrs = CommonConf.getLineMarkersDict()
    mrkrsize = CommonConf.getLineMarkersSizeDict()

    xs = np.asarray([float(i+1)/budget for i in range(budget)])
    width = 1.
    print xs
    axeslist = []
    print axes
    for ax in axes:
        print ax
        axeslist = axeslist + list(ax)
    for ax,solver in zip(axeslist, sorted(solvers)):
        zorder = 1
        pdf = pdfs[solver]
        print solver, pdf
        ys = []
        for x in xs:
            y = sum([pdf[k] for k in pdf.keys() if k <= (x+0.001) and k >=
                     (x-0.24)]) #(k > (x-min_x)
            ys.append(y)
        print sum(ys)
        print ys
        ax.bar(np.arange(len(xs))-0.5, ys, width,
               alpha=1.0,
               color=[colors[solver]]*len(xs),
               edgecolor='white',#[colors[solver]]*len(xs),
               label=CommonConf.gen_label(solver),
               linewidth=0,#linewidth[solver],
               linestyle='solid',
               #marker=mrkrs[solver],
               #markersize=mrkrsize[solver],
               zorder=1/np.mean(ys))
        ax.spines['right'].set_visible(False)
        ax.spines['top'].set_visible(False)
        ax.yaxis.set_ticks_position('left')
        ax.xaxis.set_ticks_position('bottom')
        ax.locator_params(axis='x', nbins=4)
        ax.locator_params(axis='y', nbins=4)
        ax.set_xlabel("Risk");
        ax.set_ylabel("Probability");
        ax.legend(loc='best', borderaxespad=0., fancybox=True, ncol=3)
    pp.locator_params(axis='x', nbins=6)
    pp.xticks(np.arange(len(xs)), xs)
    pp.xlim(-0.5,3.5)
    pp.tight_layout(pad=0)
    pp.savefig("tmp.pdf")


def parse_file(file_name):
    pdf = dict()
    iters = dict()
    with open(file_name) as f:
        alg =""
        for l in f.readlines():
            if ":" in l:
                tkns = l.split(" ")
                x = float(tkns[0])
                y = float(tkns[2])
                pdf[alg][x] = pdf[alg].get(x,0) + y
            elif len(l) > 1:
                alg = l.strip()
                if alg not in pdf.keys():
                    pdf[alg]=dict()
                iters[alg]=iters.get(alg, 0)+1
    for alg,dist in pdf.iteritems():
        for k,v in dist.iteritems():
            pdf[alg][k] = v/iters[alg]
    return pdf

def print_dict(d):
    for k,v in d.iteritems():
        print k,"\t",v

def print_pdf(pdf):
    for alg,dist in pdf.iteritems():
        print "\n",alg
        for k,v in dist.iteritems():
            print k,"\t",v

if __name__=="__main__":
    pdf = parse_file(sys.argv[1])
    budget = int(sys.argv[2])
    out_file = sys.argv[3]
    plot_pdf(pdf, budget, out_file)
