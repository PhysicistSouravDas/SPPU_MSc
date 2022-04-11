#!/usr/bin/env gnuplot
set terminal 'qt' font 'Helvetica,18' #size 1800, 1000
set title 'Charge V(t) vs t'
set xlabel 'Time(t) (second)'
set ylabel 'V(t) (volt)'
set key at 24.3, 110
set key box width 0 height 1 opaque
set key spacing 1
set key box dt 5
p 'output.txt' u 1:5 w p pointtype 35 title "Voltage, V(t)"

# In terminal, run
# ~$ 'gnuplot -persist gnuplot_script7.gnu'
# Without the quotes. Plot will be displayed if gnuplot is installed
