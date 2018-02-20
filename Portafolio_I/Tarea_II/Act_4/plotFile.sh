#!/bin/bash
long=4.31
gnuplot -persist <<-EOFMarker
    set title "Configuracion Inicial (N= 512,n=0.8,sigma=1,L=8.617)"
    set size square
    set xrange[-$long:$long]
    set yrange[-$long:$long]
    set zrange[-$long:$long]
    set style line 1 lc rgb '#0060ad' lt 1 lw 1 pt 7 ps 1
    set key off
    set ticslevel 0
    set view equal xyz
    splot "PosPart.dat" u 1:2:3:(0.5) w circles ls 1
EOFMarker
