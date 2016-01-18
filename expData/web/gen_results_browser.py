import sys
import os
print 'Results for: <br>'
for topo in next(os.walk(sys.argv[1]))[1]:
    if topo not in ['web']:
        print '<a href='+topo+'/index.html>'+topo+'<br>'
