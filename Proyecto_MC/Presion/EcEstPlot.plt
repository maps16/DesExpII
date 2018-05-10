#ESTILOS DE LAS GRAFICAS
set style line 1  lc rgb "black"  lw 2.000 pt 7 ps 1.000
set style line 2  lc rgb "red"  lw 1.000 pt 7 ps 1.000
set style line 3  lc rgb "blue"  lw 2.000 pt 7 ps 1.000

#INFORMACION EN PLOT
#set key off
set xlabel "Desnsidad(n*)"
set ylabel "Presion (P*)"
set xrange [0:1]
set xtics 0.1
set title "Ecuacion de Estado"

#FUNCIONES A PLOT
l(x) = x*(1 + (3.1416/6)*x +(3.1416/6)**2 * x**2 - (3.1416/6)**3 * x**3) #CARNAHAN-STARLING TOP
s(x) = (1-(3.1416/6)*x)**3                                               #CARNAHAN-STARLING BOTTOM
f(x)= l(x) / s(x)							 #CARNAHAN-STARLING
gh(x)= x     								 #GAS IDEAL

#PLOT
plot "Presion.dat" u 1:2 w points ls 2 title "HS",\
f(x) w lines  ls 1 title "Carnahan-Starling",\
gh(x) w lines ls 3 title "Gas Ideal"



