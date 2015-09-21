#!/bin/sh
nl=10000
echo "spf"
./parse-stats-withouttime.sh spf-stats.txt
#nl=`wc -l sw-* | awk '{print $1}' | sort | head -n 1 `

python ./paste.py sw-1-sw-2.txt sw-2-sw-1.txt sw-2-sw-5.txt sw-5-sw-2.txt sw-2-sw-6.txt sw-6-sw-2.txt sw-2-sw-12.txt sw-12-sw-2.txt sw-3-sw-6.txt sw-6-sw-3.txt sw-3-sw-9.txt sw-9-sw-3.txt sw-4-sw-7.txt sw-7-sw-4.txt sw-4-sw-10.txt sw-10-sw-4.txt sw-4-sw-11.txt sw-11-sw-4.txt sw-5-sw-7.txt sw-7-sw-5.txt sw-5-sw-8.txt sw-8-sw-5.txt sw-6-sw-7.txt sw-7-sw-6.txt sw-8-sw-10.txt sw-10-sw-8.txt sw-9-sw-12.txt sw-12-sw-9.txt sw-10-sw-11.txt sw-11-sw-10.txt  > tmp
head -n $nl tmp > all-switch-load-timeline.txt
python getstats.py > spf-cr.txt

echo "ecmp"
./parse-stats-withouttime.sh ecmp-stats.txt
#nl=`wc -l sw-* | awk '{print $1}' | sort | head -n 1 `
python ./paste.py sw-1-sw-2.txt sw-2-sw-1.txt sw-2-sw-5.txt sw-5-sw-2.txt sw-2-sw-6.txt sw-6-sw-2.txt sw-2-sw-12.txt sw-12-sw-2.txt sw-3-sw-6.txt sw-6-sw-3.txt sw-3-sw-9.txt sw-9-sw-3.txt sw-4-sw-7.txt sw-7-sw-4.txt sw-4-sw-10.txt sw-10-sw-4.txt sw-4-sw-11.txt sw-11-sw-4.txt sw-5-sw-7.txt sw-7-sw-5.txt sw-5-sw-8.txt sw-8-sw-5.txt sw-6-sw-7.txt sw-7-sw-6.txt sw-8-sw-10.txt sw-10-sw-8.txt sw-9-sw-12.txt sw-12-sw-9.txt sw-10-sw-11.txt sw-11-sw-10.txt  > tmp
head -n $nl tmp > all-switch-load-timeline.txt
python getstats.py > ecmp-cr.txt

#echo "obliv"
#./parse-stats-withouttime.sh obliv-stats.txt
#nl=`wc -l sw-* | awk '{print $1}' | sort | head -n 1 `


#python ./paste.py sw-1-sw-2.txt sw-2-sw-1.txt sw-2-sw-5.txt sw-5-sw-2.txt sw-2-sw-6.txt sw-6-sw-2.txt sw-2-sw-12.txt sw-12-sw-2.txt sw-3-sw-6.txt sw-6-sw-3.txt sw-3-sw-9.txt sw-9-sw-3.txt sw-4-sw-7.txt sw-7-sw-4.txt sw-4-sw-10.txt sw-10-sw-4.txt sw-4-sw-11.txt sw-11-sw-4.txt sw-5-sw-7.txt sw-7-sw-5.txt sw-5-sw-8.txt sw-8-sw-5.txt sw-6-sw-7.txt sw-7-sw-6.txt sw-8-sw-10.txt sw-10-sw-8.txt sw-9-sw-12.txt sw-12-sw-9.txt sw-10-sw-11.txt sw-11-sw-10.txt  > tmp
#head -n $nl tmp > all-switch-load-timeline.txt
#python getstats.py > obliv-cr.txt

echo "ak"
./parse-stats-withouttime.sh ak-stats.txt
#nl=`wc -l sw-* | awk '{print $1}' | sort | head -n 1 `


python paste.py sw-1-sw-2.txt sw-2-sw-1.txt sw-2-sw-5.txt sw-5-sw-2.txt sw-2-sw-6.txt sw-6-sw-2.txt sw-2-sw-12.txt sw-12-sw-2.txt sw-3-sw-6.txt sw-6-sw-3.txt sw-3-sw-9.txt sw-9-sw-3.txt sw-4-sw-7.txt sw-7-sw-4.txt sw-4-sw-10.txt sw-10-sw-4.txt sw-4-sw-11.txt sw-11-sw-4.txt sw-5-sw-7.txt sw-7-sw-5.txt sw-5-sw-8.txt sw-8-sw-5.txt sw-6-sw-7.txt sw-7-sw-6.txt sw-8-sw-10.txt sw-10-sw-8.txt sw-9-sw-12.txt sw-12-sw-9.txt sw-10-sw-11.txt sw-11-sw-10.txt  > tmp
head -n $nl tmp > all-switch-load-timeline.txt
python getstats.py > ak-cr.txt

echo "mcf"
./parse-stats-withouttime.sh mcf-stats.txt
#nl=`wc -l sw-* | awk '{print $1}' | sort | head -n 1 `


python paste.py sw-1-sw-2.txt sw-2-sw-1.txt sw-2-sw-5.txt sw-5-sw-2.txt sw-2-sw-6.txt sw-6-sw-2.txt sw-2-sw-12.txt sw-12-sw-2.txt sw-3-sw-6.txt sw-6-sw-3.txt sw-3-sw-9.txt sw-9-sw-3.txt sw-4-sw-7.txt sw-7-sw-4.txt sw-4-sw-10.txt sw-10-sw-4.txt sw-4-sw-11.txt sw-11-sw-4.txt sw-5-sw-7.txt sw-7-sw-5.txt sw-5-sw-8.txt sw-8-sw-5.txt sw-6-sw-7.txt sw-7-sw-6.txt sw-8-sw-10.txt sw-10-sw-8.txt sw-9-sw-12.txt sw-12-sw-9.txt sw-10-sw-11.txt sw-11-sw-10.txt  > tmp
head -n $nl tmp > all-switch-load-timeline.txt
python getstats.py > mcf-cr.txt
rm tmp
