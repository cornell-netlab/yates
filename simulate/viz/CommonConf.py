import matplotlib.pyplot as pp

def setupMPPDefaults():
    pp.rcParams['font.size'] = 13
    pp.rcParams['ytick.labelsize'] = 15
    pp.rcParams['xtick.labelsize'] = 15
    pp.rcParams['legend.fontsize'] = 13
    pp.rcParams['lines.markersize'] = 8
    pp.rcParams['axes.titlesize'] = 18
    pp.rcParams['axes.labelsize'] = 15

def getLineMarkers():
  #return ['+-', '*-', 'x-', '<-', '>-', 'o-', 's-', 'd-', '^-', 'v-']
    # return [ r'$\lambda$',
    #          r'$\bowtie$',
    #          r'$\circlearrowleft$',
    #          r'$\clubsuit$',
    #          r'$\checkmark$',
    #           '<', '>']
    return ['s', 'o', '<', '>', '^', 'v', 'd', '^', 'v']

def getLineFormats():
    return [':',':','-', '-', '--', '--', '-', '-', '-', '-']
