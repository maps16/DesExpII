#!/bin/bash
gnuplot -persist <<-EOFMarker
    set style line 1 lc rgb '#000000' lt 1 lw 1 pt 7 ps 1
    set style line 2 lc rgb '#785896' lt 1 lw 1 pt 7 ps 1
    plot "Part1.dat" u 1:2 w linespoints ls 1 title "Particula 1", "Part2.dat" u 1:2 w linespoints ls 2 title "Particula 2"
    set size square
    set object 1 rect from -7.17,-7.17 to 7.17,7.17 lw 2
    replot
EOFMarker
