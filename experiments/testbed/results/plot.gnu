#!/usr/bin/gnuplot 
set terminal postscript eps enhanced color dashed lw 1 'Helvetica' 4
set output 'load-timeline.eps'
set title "Traffic on each edge - Abilene topology"
set ylabel "TX bytes in an interval"
set xlabel "Time (s)"
set xrange [0:200]
#set yrange [0:65500000]

samples(x) = $0 > 4 ? 5 : ($0+1)
sum5(x) = (shift5(x), (back1+back2+back3+back4+back5))
shift5(x) = (back5 = back4, back4 = back3, back3 = back2, back2 = back1, back1 = x)
init(x) = (back1 = back2 = back3 = back4 = back5 = sum = 0)
set multiplot layout 6,6 rowsfirst
set label 1 'a' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-1-sw-2.txt" using 1:(sum5($2)) every 5 title "sw-1-sw-2.txt" with lines lt 1
set label 1 'b' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-2-sw-1.txt" using 1:(sum5($2)) every 5 title "sw-2-sw-1.txt" with lines lt 1
set label 1 'c' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-2-sw-5.txt" using 1:(sum5($2)) every 5 title "sw-2-sw-5.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-2-sw-6.txt" using 1:(sum5($2)) every 5 title "sw-2-sw-6.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-2-sw-12.txt" using 1:(sum5($2)) every 5 title "sw-2-sw-12.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-3-sw-6.txt" using 1:(sum5($2)) every 5 title "sw-3-sw-6.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-3-sw-9.txt" using 1:(sum5($2)) every 5 title "sw-3-sw-9.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-4-sw-7.txt" using 1:(sum5($2)) every 5 title "sw-4-sw-7.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-4-sw-10.txt" using 1:(sum5($2)) every 5 title "sw-4-sw-10.txt" with lines lt 1
plot sum = init(0),  312500000, \
 "sw-4-sw-11.txt" using 1:(sum5($2)) every 5 title "sw-4-sw-11.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-5-sw-2.txt" using 1:(sum5($2)) every 5 title "sw-5-sw-2.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-5-sw-7.txt" using 1:(sum5($2)) every 5 title "sw-5-sw-7.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-5-sw-8.txt" using 1:(sum5($2)) every 5 title "sw-5-sw-8.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-6-sw-2.txt" using 1:(sum5($2)) every 5 title "sw-6-sw-2.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-6-sw-3.txt" using 1:(sum5($2)) every 5 title "sw-6-sw-3.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-6-sw-7.txt" using 1:(sum5($2)) every 5 title "sw-6-sw-7.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-7-sw-4.txt" using 1:(sum5($2)) every 5 title "sw-7-sw-4.txt" with lines lt 1
plot sum = init(0),  312500000, \
 "sw-7-sw-5.txt" using 1:(sum5($2)) every 5 title "sw-7-sw-5.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-7-sw-6.txt" using 1:(sum5($2)) every 5 title "sw-7-sw-6.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-8-sw-5.txt" using 1:(sum5($2)) every 5 title "sw-8-sw-5.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-8-sw-10.txt" using 1:(sum5($2)) every 5 title "sw-8-sw-10.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-9-sw-3.txt" using 1:(sum5($2)) every 5 title "sw-9-sw-3.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-9-sw-12.txt" using 1:(sum5($2)) every 5 title "sw-9-sw-12.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-10-sw-4.txt" using 1:(sum5($2)) every 5 title "sw-10-sw-4.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-10-sw-8.txt" using 1:(sum5($2)) every 5 title "sw-10-sw-8.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-10-sw-11.txt" using 1:(sum5($2)) every 5 title "sw-10-sw-11.txt" with lines lt 1

set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-11-sw-4.txt" using 1:(sum5($2)) every 5 title "sw-11-sw-4.txt" with lines lt 1

set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-11-sw-10.txt" using 1:(sum5($2)) every 5 title "sw-11-sw-10.txt" with lines lt 1
set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-12-sw-2.txt" using 1:(sum5($2)) every 5 title "sw-12-sw-2.txt" with lines lt 1

set label 1 'd' at graph 0.92,0.9 font ',4'
plot sum = init(0),  312500000, \
 "sw-12-sw-9.txt" using 1:(sum5($2)) every 5 title "sw-12-sw-9.txt" with lines lt 1

reset

