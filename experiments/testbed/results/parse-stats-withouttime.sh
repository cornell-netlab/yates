#!/bin/sh
rm sw-*
cp $1 stats.txt
# extract time and tx_bytes
cat stats.txt | grep "switch=1, port_no=2," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- - > sw-1-sw-2.txt 

cat stats.txt | grep "switch=2, port_no=14," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-2-sw-1.txt 
cat stats.txt | grep "switch=2, port_no=15," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-2-sw-5.txt 
cat stats.txt | grep "switch=2, port_no=16," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-2-sw-6.txt 
cat stats.txt | grep "switch=2, port_no=17," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-2-sw-12.txt 

cat stats.txt | grep "switch=3, port_no=26," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-3-sw-6.txt 
cat stats.txt | grep "switch=3, port_no=27," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-3-sw-9.txt 

cat stats.txt | grep "switch=4, port_no=38," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-4-sw-7.txt 
cat stats.txt | grep "switch=4, port_no=39," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-4-sw-10.txt 
cat stats.txt | grep "switch=4, port_no=40," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-4-sw-11.txt 

cat stats.txt | grep "switch=5, port_no=2," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-5-sw-2.txt 
cat stats.txt | grep "switch=5, port_no=3," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp | cut -d' ' -f2- -  >sw-5-sw-7.txt 
cat stats.txt | grep "switch=5, port_no=11," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-5-sw-8.txt 

cat stats.txt | grep "switch=6, port_no=14," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-6-sw-2.txt 
cat stats.txt | grep "switch=6, port_no=15," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-6-sw-3.txt 
cat stats.txt | grep "switch=6, port_no=16," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-6-sw-7.txt 

cat stats.txt | grep "switch=7, port_no=26," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-7-sw-4.txt 
cat stats.txt | grep "switch=7, port_no=27," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-7-sw-5.txt 
cat stats.txt | grep "switch=7, port_no=28," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-7-sw-6.txt 

cat stats.txt | grep "switch=8, port_no=38," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-8-sw-5.txt 
cat stats.txt | grep "switch=8, port_no=39," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-8-sw-10.txt 

cat stats.txt | grep "switch=9, port_no=2," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-9-sw-3.txt 
cat stats.txt | grep "switch=9, port_no=11," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-9-sw-12.txt 

cat stats.txt | grep "switch=10, port_no=14," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-10-sw-4.txt 
cat stats.txt | grep "switch=10, port_no=15," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-10-sw-8.txt 
cat stats.txt | grep "switch=10, port_no=16," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-10-sw-11.txt 

cat stats.txt | grep "switch=11, port_no=26," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-11-sw-4.txt 
cat stats.txt | grep "switch=11, port_no=27," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-11-sw-10.txt 

cat stats.txt | grep "switch=12, port_no=38," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-12-sw-2.txt 
cat stats.txt | grep "switch=12, port_no=39," | sed 's/=/ /g' | sed 's/,//g' | awk '{print $2 " " $14}' |  sed 's/......... / /' > tmp && ./sort.py -i tmp  | cut -d' ' -f2- - >sw-12-sw-9.txt 
rm tmp
