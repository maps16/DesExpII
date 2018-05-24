set style line 1 lc rgb "black" lw 1 pt 7 ps 1
set style line 2 lc rgb "blue" lw 1 pt 7 ps 0.75

set size square
set title "G(r) para diferente configuraciones Iniciales n*=0.5, Nstep=30,000"

plot "gdr05Ale.dat" u 1:2 w points ls 1 title "Aleatoria",\
"gdr05Reg.dat" u 1:2 w points ls 2 title "Regular"