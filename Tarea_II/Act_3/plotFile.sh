#!/bin/bash
gnuplot -persist <<-EOFMarker
    set title "Configuracion Inicial (N=100,n=0.5,sigma=1)"
    set size square
    set xrange[-59.77:59.77]
    set yrange[-7.08:7.08]
    set style line 1 lc rgb '#0060ad' lt 1 lw 1 pt 7 ps 1
    set key off 
    plot "PosPart.dat" u 1:2:(0.5) w circles
EOFMarker

