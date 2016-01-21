import sys
import os
print 'Results for: <br>'
tl = []
for topo in next(os.walk(sys.argv[1]))[1]:
    if topo not in ['web']:
        tl.append(topo)

for topo in sorted(tl):
        print '<a href='+topo+'/index.html>'+topo+'<br>'
