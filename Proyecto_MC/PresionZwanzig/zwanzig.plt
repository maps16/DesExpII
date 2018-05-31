set style line 1 lc "blue" lw 1 pt 7 ps 1
set style line 2 lc "black" lw 1 pt 7 ps 1
set style line 3 lc "red" lw 1 pt 7 ps 1
set style line 4 lc "green" lw 1 pt 7 ps 1
#Gas Ideal
f(x) = x
#Van Der Waals
v(x) = x/(1-x*2.0943) - x*x*2.6976

plot "PrssZwanzig1.dat" u 1:2 w points ls 1 title "T=1.00" , \
"PrssZwanzig74.dat" u 1:2 w points ls 2 title "T=0.74"
#"Presion.dat" u 1:2 w points ls 2 title "HS" ,\
#f(x) ls 3 title "Gas Ideal" , \
#v(x) ls 4 title "Van Der Waals"
#plot "PrssZwanzig1.dat" u 1:2 w points ls 1 title "T=1.00" , \