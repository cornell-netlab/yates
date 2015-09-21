#!/usr/bin/gnuplot 
set terminal postscript eps enhanced color dashed lw 1 'Helvetica' 4
set output 'flow-sizes.eps'
set title "Flow size distribution - Abilene"
set ylabel "CDF"
set xlabel "Flow size(B)"
set logscale x
plot "dist.txt" using 1:2 every 100 with lines lt 1

reset

