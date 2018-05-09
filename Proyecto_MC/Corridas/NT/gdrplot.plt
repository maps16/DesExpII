set style line 1  linecolor rgb "black"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 2  linecolor rgb "#fa0000"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 3  linecolor rgb "#1100ff"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 4  linecolor rgb "#ffab00"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 5  linecolor rgb "#ffabff"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 6  linecolor rgb "green"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 7  linecolor rgb "#996600"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 8  linecolor rgb "#66F1FF"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 9  linecolor rgb "#9f9f9f"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 
set style line 10  linecolor rgb "#ffcc66"  linewidth 1.000 dashtype solid pointtype 7 pointsize 1.000 

plot "gdr01.dat" u 1:2 w points ls 1title "GDR01" , \
"gdr02.dat" u 1:2 w points ls 2 title "GDR02" , \
"gdr03.dat" u 1:2 w points ls 3 title "GDR03" , \
"gdr04.dat" u 1:2 w points ls 4 title "GDR04", \
"gdr05.dat" u 1:2 w points ls 5 title "GDR05", \
"gdr06.dat" u 1:2 w points ls 6 title "GDR06", \
"gdr07.dat" u 1:2 w points ls 7 title "GDR07", \
"gdr08.dat" u 1:2 w points ls 8 title "GDR08", \
"gdr09.dat" u 1:2 w points ls 9 title "GDR09"
#plot "gdr10.dat" u 1:2 w points ls 10 title "GDR10"

