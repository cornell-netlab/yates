import sys

if __name__ == "__main__":
  if len(sys.argv) < 2:
    print "usage: ./%s [files] ..." % (sys.argv[0])
  files = sys.argv[1:]

  def list_of_file(f):
    l = []
    lines = open(f)
    for line in lines:
      ls = line.strip()
      if len(ls) == 0:
        continue
      l.append(float(ls))
    return l

  all_lines = map(list_of_file, files)

  max_len = 0
  for lst in all_lines:
    if len(lst) > max_len:
      max_len = len(lst)

  def pad_with_zero(lst):
    while len(lst) < max_len:
      lst.append(0.0)

  for l in all_lines:
    pad_with_zero(l)

  summed_lines = all_lines
  i = 0

  for i in range(max_len):
    values = map(lambda x: x[i], summed_lines)
    s = ""
    for v in values:
      s = s + ("%f " % v)
    print s
