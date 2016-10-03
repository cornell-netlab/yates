
import re
from collections import OrderedDict

def parseData(dirn, fname, solvers=set()):
  xs = []
  ysPerSolver = OrderedDict()
  ydevsPerSolver = OrderedDict()
  with open(dirn+"/"+fname+".dat") as fin:
    lines = fin.readlines()
    for line in lines:
      if line.startswith("#"):
        continue
      if line.startswith("\n"):
        continue
      # print line
      (solver, x, y, ydev) = re.split("[\t]", line)
      if (not solvers) or (solver in solvers):
        x = float(x)
        try:
          y = float(y)
          ydev = float(ydev)
        except ValueError:
          y = 0.0
          ydev = 0.0
        if solver in ysPerSolver:
          ysPerSolver[solver].append(y)
          ydevsPerSolver[solver].append(ydev)
        else:
          ysPerSolver[solver] = [y]
          ydevsPerSolver[solver] = [ydev]
        if len(xs) == 0 or x > xs[-1]:
          xs.append(x)
    return (xs, ysPerSolver, ydevsPerSolver)


def get_scheme_congestions (all_congestions):
    solver_conglist = dict()
    for scheme,scm_congestion in all_congestions.iteritems():
        conglist = []
        for iteration, congs in scm_congestion.iteritems():
            for l,c in congs.iteritems():
                if 'h' not in l:
                    conglist.append(c)
        solver_conglist[scheme] = conglist
    return solver_conglist

def parse_congestions_file (filename):
    all_congestions = OrderedDict() # solver -> iter -> edge -> cong
    scheme = ""
    iteration = ""
    links = dict()
    with open(filename) as f:
        for line in f.readlines():
            if line[0] == "#":
                continue
            else:
                tokens = line.split('\t')
                if len(tokens) != 3:
                    if len(links) != 0:
                        all_congestions[scheme][int(iteration)] = links
                        links = dict()
                    continue
                if tokens[2] == '\n':
                    if len(links) != 0:
                        all_congestions[scheme][int(iteration)] = links
                        links = dict()
                    scheme = tokens[0]
                    iteration = tokens[1]
                    if scheme not in all_congestions.keys():
                        all_congestions[scheme] = dict()
                else:
                    edge_cr = tokens[2].split(':')
                    links[edge_cr[0].strip()] = float(edge_cr[1].strip())
    return get_scheme_congestions(all_congestions) # scheme -> cong list

