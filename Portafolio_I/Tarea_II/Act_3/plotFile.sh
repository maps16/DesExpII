#!/bin/bash
long=13.83
gnuplot -persist <<-EOFMarker
    set title "Configuracion Inicial (N=512,n=0.67,sigma=1,L=27.643)"
    set size square
    set xrange[-$long:$long]
    set yrange[-$long:$long]
    set zrange[-$long:$long]
    set style line 1 lc rgb '#0060ad' lt 1 lw 1 pt 7 ps 1
    set key off 
    plot "PosPart.dat" u 1:2:(0.5) w circles ls 1
EOFMarker

