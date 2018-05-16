set style line 1 lc rgb "red" lw 1.0 pt 7 ps 1.50
set style line 2 lc rgb "blue" lw 1.0 pt 7 ps 0.50

set size square
set title "Configurcion Regular"
set key left top

plot "Terma.dat" u 1:2 w points ls 1 title "n*=0.5",\
"Terma001.dat" u 1:2 w points ls 2 title "n*=0.1"