#!/bin/bash
./parse-stats.sh spf-stats.txt 
./plot.gnu 
mv load-timeline.eps load-timeline-spf.eps
# ./parse-stats.sh ecmp-stats.txt 
# ./plot.gnu 
# mv load-timeline.eps load-timeline-ecmp.eps
# ./parse-stats.sh mcf-stats.txt 
# ./plot.gnu 
# mv load-timeline.eps load-timeline-mcf.eps
# rm sw-*
# scp load-timeline-*.eps  praveenk@titan:~/
