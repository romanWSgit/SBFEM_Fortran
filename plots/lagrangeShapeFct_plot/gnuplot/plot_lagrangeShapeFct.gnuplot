#!/usr/bin/gnuplot
#
# Creates a version of a plot, which looks nice for inclusion on web pages
#
# AUTHOR: Roman Wallner- Silberhuber

print(script_path)
set terminal pdfcairo enhanced color dashed font "Alegreya, 14" \
rounded size 16 cm, 9.6 cm

# Default encoding, line styles, pallette, border and grid are set in
# /usr/local/share/gnuplot/x.y/gnuplotrc.

set xlabel "x"
set ylabel "f(x)"
set grid
set key outside c r 
set xrange[-1:1]
#set yrange[0:1]
set grid
set title "Lagrangian Shape functions"


#set term pngcairo
# set term qt
#set nokey


set output script_path . '../../images/lagrangeShapeFct.pdf'
stats script_path . "../../data/lagrangeShapeFct.dat" nooutput
N = STATS_records
M = STATS_columns
data= script_path . "../../data/lagrangeShapeFct.dat"
#plot m using 1:2 with lp
#plot data   u 1:2 w l ls 1 t "shape1" , \
#    ''   u 1:3  w l ls 2 t "shape2" 
plot for [i=2:M] data using 1:i w l ls (i-1) lw 3 title "shape ".(i-1)

# set term xterm
#set terminal qt enhanced font "Alegreya, 14" persist

#plot for [i=2:M] data using 1:i w l ls (i-1)  title "shape ".(i-1) 

#set terminal wxt enhanced font 'Verdana,9' title "plot" persist

#plot for [i=2:M] data using 1:i w l ls (i-1)  title "shape ".(i-1)
#pause 5
#reread

