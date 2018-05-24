set style line 1  lc rgb "black"  lw 1.000 pt 7 ps 1.000 
set style line 2  lc rgb "#fa0000"  lw 1.000 pt 7 ps 1.000 
set style line 3  lc rgb "#1100ff"  lw 1.000 pt 7 ps 1.000 
set style line 4  lc rgb "#ffab00"  lw 1.000 pt 7 ps 1.000 
set style line 5  lc rgb "#ffabff"  lw 1.000 pt 7 ps 1.000 
set style line 6  lc rgb "green"  lw 1.000 pt 7 ps 1.000 
set style line 7  lc rgb "#996600"  lw 1.000 pt 7 ps 1.000 
set style line 8  lc rgb "#66F1FF"  lw 1.000 pt 7 ps 1.000 
set style line 9  lc rgb "#9f9f9f"  lw 1.000 pt 7 ps 1.000 
set style line 10  lc rgb "#ffcc66"  lw 1.000 pt 7 ps 1.000 

plot "gdr01.dat" u 1:2 w points ls 1title "GDR01" , \
"gdr02.dat" u 1:2 w points ls 2 title "GDR02" , \
"gdr03.dat" u 1:2 w points ls 3 title "GDR03" , \
"gdr04.dat" u 1:2 w points ls 4 title "GDR04", \
"gdr05.dat" u 1:2 w points ls 5 title "GDR05", \
"gdr06.dat" u 1:2 w points ls 6 title "GDR06", \
"gdr07.dat" u 1:2 w points ls 7 title "GDR07", \
"gdr08.dat" u 1:2 w points ls 8 title "GDR08", \
"gdr09.dat" u 1:2 w points ls 9 title "GDR09", \
"gdr10.dat" u 1:2 w points ls 10 title "GDR10"
plot "gdr10.dat" u 1:2 w points ls 10 title "GDR10"
