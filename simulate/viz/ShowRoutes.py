import pandas as pd
import numpy as np
import math as math
import matplotlib.pyplot as plt
proper_name = {
        'ecmp' : 'ECMP',
        'ksp' : 'KSP',
        'mcf' : 'MCF',
        'raeke' : 'Racke',
        'semimcfecmp' : 'Adaptive ECMP',
        'semimcfksp' : 'SWAN',
        'semimcfraeke' : 'SOTE',
        'semimcfmcfenv' : 'Adaptive MCF',
        'semimcfmcfftenv' : 'Joint',
        'semimcfvlb' : 'Adaptive VLB',
        'spf' : 'SPF',
        'vlb' : 'VLB',
        }
def getKey(item):
    return item[0]
def plotsubfig(fig,hosts,edges,alg, scheme):
    maxx=20
    maxy=10
    colors=['red','green','purple']
    fig.set_title(proper_name[alg],fontsize=24)
    for e in edges:
        a,b=e
        fig.plot([hosts[a-1][0],hosts[b-1][0]], [hosts[a-1][1],hosts[b-1][1]],color='blue',linestyle='-.' ,
                 linewidth=0.6)
    np=0
    for path in sorted(scheme,key=getKey,reverse=True):
        prob=path[0]
        if (prob<0.1):
            prob=0.1
        for entry in range(1,len(path)-1):
            a=path[entry]
            b=path[entry+1]
            fig.plot([hosts[a-1][0],hosts[b-1][0]], [hosts[a-1][1],hosts[b-1][1]],
                     colors[np],linewidth=5*prob+1.5,alpha=min(0.7,prob*3))
        np+=1
    for h in hosts:
        fig.plot(h[0], h[1], 'black',marker='o')
        fig.text(h[0]+h[3],h[1]+h[4], h[2], fontsize=20)

    fig.axis([0, maxx, 0, maxy])
    fig.axis('off')
    for tic in fig.xaxis.get_major_ticks():
        tic.tick1On = tic.tick2On = False
        tic.label1On = tic.label2On = False
    for tic in fig.yaxis.get_major_ticks():
        tic.tick1On = tic.tick2On = False
        tic.label1On = tic.label2On = False

with open('routes.txt') as f:
    content = f.readlines()
last_alg=""
picked=["h4","h2"]
printnextblock=False
gotfirst=False
scheme_map={}
for line in content:
    if printnextblock:
        if not "@" in line:
            printnextblock=False
            gotfirst=True
        #print(line)
        if ("@" in line):
            probs=float(line.split("@")[1])
            #print(probs)
            scheme_map[cur_alg].append([probs])
            paths=line[line.find('[')+1:line.find(']')].split(", ")
            for entry in paths[1:]:
                [pre,nex]=entry[1:-1].split(",")
                #print(pre,"->",nex)
                scheme_map[cur_alg][-1].append(int(pre[1:]))
            #print("")
    if ("Algo" in line) & (not 'optimal' in line) & (not 'semimcfraekeft' in line):
        eles=line.split(" ")
        cur_alg=eles[1].replace("\n","")
        if (cur_alg!=last_alg):
            #if (last_alg!=""):
                #print(scheme_map[last_alg])
            #print(cur_alg)
            last_alg=cur_alg
            scheme_map[cur_alg]=[]
            gotfirst=False
    if (picked[0]+" ->" in line) and (picked[1] in line) and (not gotfirst):
        printnextblock=True
        #print(line)


edges=[
    [1,2],
    [2,6],
    [2,12],
    [2,5],
    [3,9],
    [3,6],
    [4,7],
    [4,11],
    [4,10],
    [5,7],
    [5,8],
    [6,7],
    [8,10],
    [9,12],
    [10,11]
]
hosts=[
    [16.5,3.5,' 1',0,0],
    [15.9,4.1,' 2',-1.6,0.3],
    [13.2,7.5,' 3',0,0.4],
    [7.2,6,' 4',0,0.3],
    [11,2.2,' 5',0.4,-0.7],
    [14.5,6.5,' 6',0,0],
    [11.2,6.1,' 7',-1.0,0.3],
    [2,3.8,' 8',-0.5,-1.2],
    [19.2,8.0,' 9',0,0],
    [0.8,6,' 10',0,0.3],
    [2,8.5,' 11',0,0],
    [18.2,6.4,' 12',0,-0.4],
]

totRow=2
totCol=6
f,figs=plt.subplots(totRow, totCol,figsize=(28,7))
counter=0
i=0
j=0
for c in sorted(scheme_map):
    print(c,scheme_map[c])
    plotsubfig(figs[i][j],hosts,edges,c,scheme_map[c])
    j=j+1
    if j==totCol:
        j=0
        i=i+1
#plt.show()
plt.savefig("routingschemes.pdf",bbox_inches='tight', pad_inches=0)
