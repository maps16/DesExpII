set style line 1 lc "blue" lw 1 pt 7 ps 1
set style line 2 lc "black" lw 1 pt 7 ps 1
set style line 3 lc "red" lw 1 pt 7 ps 1
set style line 4 lc "green" lw 1 pt 7 ps 1
set style line 5 lc "purple" lw 1 pt 7 ps 1
#Gas Ideal
f(x) = x
#Van Der Waals
v(x) = x/(1-x*2.0943) - x*x*2.6976 #T*=0.74
c(x) = x/(1-x*2.0943) - x*x*1.9962 #T*=1.0

#set size square
set yrange [0:14]
set key top left
set title "Presion a T^*=0.74" font "Helvetica,12"
set xlabe "n^*" font "Helvetica,12"
set ylabe "P^*" font "Helvetica,12"

plot "PrssZwanzig74.dat" u 1:2 w points ls 1 title "Zwanzig" , \
"Presion.dat" u 1:2 w points ls 2 title "HS" ,\
f(x) ls 3 title "Gas Ideal" , \
v(x) ls 4 title "Van Der Waals" ,\
"PresionT10.dat" u 1:2 w points ls 5 title "SW"
