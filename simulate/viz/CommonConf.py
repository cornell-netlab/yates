import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as pp

def setupMPPDefaults():
    pp.rcParams['font.size'] = 13
    pp.rcParams['ytick.labelsize'] = 15
    pp.rcParams['xtick.labelsize'] = 15
    pp.rcParams['legend.fontsize'] = 13
    pp.rcParams['lines.markersize'] = 8
    pp.rcParams['axes.titlesize'] = 18
    pp.rcParams['axes.labelsize'] = 15

algs = 'ecmp ksp mcf raeke vlb semimcfecmp semimcfksp semimcfmcfenv  semimcfraeke semimcfvlb semimcfmcfftenv spf optimalmcf'.split()

def getLineMarkers():
  #return ['+-', '*-', 'x-', '<-', '>-', 'o-', 's-', 'd-', '^-', 'v-']
    # return [ r'$\lambda$',
    #          r'$\bowtie$',
    #          r'$\circlearrowleft$',
    #          r'$\clubsuit$',
    #          r'$\checkmark$',
    #           '<', '>']
    #return ['s', 'o', '<', '^', 'v', 'd', '.', '*', 'p'] *10
    return ['<', '^', 'v', '>', '<', 'h', '.', 'o', '*', 'p', 'd', 'x'] *10

def getLineColors():
    return ['r','m', 'c', 'g', 'blue'] * 2 + ['coral', 'darkred'] * 4

def getLineFormats():
    return [':']*5 + ['--']*6 + ['-.'] * 100

def getLineMarkersDict():
    res = dict()
    marks = getLineMarkers()
    for x in range(len(algs)):
        res[algs[x]] = marks[x]
    return res

def getLineFormatsDict():
    res = dict()
    marks = getLineFormats()
    for x in range(len(algs)):
        res[algs[x]] = marks[x]
    return res

def getLineColorsDict():
    res = dict()
    marks = getLineColors()
    for x in range(len(algs)):
        res[algs[x]] = marks[x]
    return res
