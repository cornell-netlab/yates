#!/bin/bash
./parse-stats.sh spf-stats.txt 
./plot-load-timeline.gnu 
mv load-timeline.eps load-timeline-spf.eps
./parse-stats.sh ecmp-stats.txt 
./plot-load-timeline.gnu 
mv load-timeline.eps load-timeline-ecmp.eps
./parse-stats.sh mcf-stats.txt 
./plot-load-timeline.gnu 
mv load-timeline.eps load-timeline-mcf.eps
./parse-stats.sh ak-stats.txt 
./plot-load-timeline.gnu 
mv load-timeline.eps load-timeline-ak.eps
# rm sw-*
scp load-timeline-*.eps  praveenk@titan:~/
