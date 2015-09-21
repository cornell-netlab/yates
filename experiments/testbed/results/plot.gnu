#!/usr/bin/gnuplot 
set terminal postscript eps enhanced color dashed lw 1 'Helvetica' 16
set output 'congestion-timeline.eps'
set title "Congestion"
set ylabel "Congestion"
set xlabel "Time (s)"
#set xrange [3500:4000]
#set yrange[0:1.2]
set size ratio 0.3
filter(x)=((x>=0.0)?(shift7(x), x):(back3+back2+back1)/3)
filter2(x)=x
#((x>=0.0)?(prev=x, x):(prev))
samples(x) = (x>6) ? 7 : (x+2)
avg7(x) = (shift7(x), (back1+back2+back3+back4+back5+back6+back7)/samples($0))
#avg7(x) = x
shift7(x) = (back7 = back6, back6 = back5, back5 = back4, back4 = back3, back3 = back2, back2 = back1, back1 = x)
init(x) = (back1 = back2 = back3 = back4 = back5 = back6 = back7 = sum = prev = 0)
plot sum = init(0), \
 "ak-mcf-cr.txt" using (avg7(filter($1))) title "ak-mcf-max" with lines lt 1 lc 4   , \
 sum = init(0), \
 "ak-mcf-cr.txt" using (avg7(filter2($2))) title "ak-mcf-median" with lines lt 2 lc 4  , \
 sum = init(0), \
 "mcf-cr.txt" using (avg7(filter($1))) title "mcf-max" with lines lt 1 lc 5  , \
 sum = init(0), \
 "mcf-cr.txt" using (avg7(filter2($2))) title "mcf-median" with lines lt 2 lc 5  , \
 sum = init(0), \
 "smcf-mcf-cr.txt" using (avg7(filter($1))) title "smcf-mcf-max" with lines lt 1 lc 6   , \
 sum = init(0), \
 "smcf-mcf-cr.txt" using (avg7(filter2($2))) title "smcf-mcf-median" with lines lt 2 lc 6   , \
 sum = init(0), \
  "spf-cr.txt" using (avg7(filter2($1))) title "spf-max" with lines lt 1 lc 1   , \
 sum = init(0), \
 "spf-cr.txt" using (avg7(filter2($2))) title "spf-median" with lines lt 2 lc 1   , \
 sum = init(0), \
  "ecmp-cr.txt" using (avg7(filter2($1))) title "ecmp-max" with lines lt 1 lc 3   , \
 sum = init(0), \
 "ecmp-cr.txt" using (avg7(filter2($2))) title "ecmp-median" with lines lt 2 lc 3   , \
 


reset

