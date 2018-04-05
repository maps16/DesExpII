#!/bin/bash
gnuplot -persist <<-EOFMarker
    set style line 1 lc rgb '#000000' lt 1 lw 1 pt 7 ps 1
    set style line 2 lc rgb '#785896' lt 1 lw 1 pt 7 ps 1
    plot "ConIni.dat" u 1:2:(0.5) w circles ls 1 title "ConfigIni", "ConFin.dat" u 1:2:(0.5) w circles ls 2 title "ConfigFin"
    set size square
    set object 1 rect from -14.14,-14.14 to 14.14,14.14 lw 5
    replot
EOFMarker