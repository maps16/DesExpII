#!/bin/bash
long=4.62
gnuplot -persist <<-EOFMarker
    set title "Configuracion Inicial (N= 512,n=0.65,sigma=1,L=9.235)"
    set size square
    set xrange[-$long:$long]
    set yrange[-$long:$long]
    set zrange[-$long:$long]
    set style line 1 lc rgb '#0060ad' lt 1 lw 1 pt 7 ps 1
    set key off 
    splot "PosPart.dat" u 1:2:3:(0.5) w circles ls 1
EOFMarker

