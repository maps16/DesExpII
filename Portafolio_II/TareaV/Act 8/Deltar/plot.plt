set style line 1 lc rgb "black" lw 1 pt 7 ps 0.75
set style line 2 lc rgb "blue" lw 1 pt 7 ps 1.0
set style line 3 lc rgb "red" lw 1 pt 7 ps 1.5

set size square
set title "G(r) para diferente Deltar en la G(r) n*=0.5, Nstep=30,000"

plot "gdr1.dat" u 1:2 w points ls 3 title "Deltar=0.1",\
"gdr01.dat" u 1:2 w points ls 2 title "Deltar=0.01"