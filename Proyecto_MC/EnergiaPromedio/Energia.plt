set style line 1 lc "blue" lw 1 pt 7 ps 1
set style line 2 lc "black" lw 1 pt 7 ps 1
set style line 3 lc "red" lw 1 pt 7 ps 1
set style line 4 lc "green" lw 1 pt 7 ps 1

#Energia Van Der Waals T*=0.74
f(x) = -2.6976 * x
#Energia Van Der Waals T*=1.00
g(x) = -1.9962 * x

set key bottom left
set xlabel "n*" font "Helvetica,10"
set ylabel "~u^-" font "Helvetica,10"
set size square
set title "Energía Media a T^*=0.74" font "Helvetica,10"

plot "EnergyPromedio.dat" u 1:3 w points ls 1 title "Zwanzig", \
"EnergiaT74.dat" u 1:2 w points ls 2 title "SW",\
f(x) ls 3 w lines  title "Van Der Waals"
#"EnergyPromedio.dat" u 1:3 w points ls 2 title "T*=0.74", \
#f(x) ls 4 w lines  title "Van Der Waals T*=0.74"