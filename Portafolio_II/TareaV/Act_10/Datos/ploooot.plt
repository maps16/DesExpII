set style line 1 lc rgb "black" lw 1 pt 7 ps 1
set style line 2 lc rgb "blue" lw 1 pt 7 ps 1
set style line 3 lc rgb "red" lw 1 pt 7 ps 1
set style line 4 lc rgb "green" lw 1 pt 7 ps 1
set style line 5 lc rgb "yellow" lw 1 pt 7 ps 1
set style line 6 lc rgb "pink" lw 1 pt 7 ps 1
set style line 7 lc rgb "orange" lw 1 pt 7 ps 1

set size square
set title "Termalizacion, NStep = 10,000, N=100"


plot "Terma001.dat" u 1:2 w points ls 1 title "0.01" , \
"Terma010.dat" u 1:2 w points ls 2 title "0.1", \
"Terma030.dat" u 1:2 w points ls 3 title "0.3", \
"Terma050.dat" u 1:2 w points ls 4 title "0.5", \
"Terma070.dat" u 1:2 w points ls 5 title "0.7", \
"Terma090.dat" u 1:2 w points ls 6 title "0.9", \
"Terma099.dat" u 1:2 w points ls 7 title "0.999"
