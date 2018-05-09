set style line 1  lc rgb "black"  lw 1.000 pt 7 ps 1.000
set key off
set xlabel "Desnsidad(n*)"
set ylabel "Presion (P*)"
show xlabel
show ylabel
set title "Ecuacion de Estado"
plot "PresVSns.dat" u 1:2 w points ls 1title "GDR01"


