set style line 1 lc rgb "black" lw 1 pt 7 ps 1
set style line 2 lc rgb "blue" lw 1 pt 7 ps 0.75
set style line 3 lc rgb "red" lw 1 pt 7 ps 1.25

set size square
set title "G(r) para diferente Cantidad de Partículas n*=0.5, Nstep=30,000, Aleatoria"

plot "gdr200.dat" u 1:2 w points ls 3 title "200",\
"gdr400.dat" u 1:2 w points ls 1 title "400", \
"gdr600.dat" u 1:2 w points ls 2 title "600"