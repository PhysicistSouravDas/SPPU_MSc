#!/usr/bin/env gnuplot
set terminal 'qt' font 'Helvetica,18' size 1800, 1000
set title 'Fitting points along an exponential function'
set xlabel 'x --->'
set ylabel 'y --->'
set key at 1.8, 29.5
set key box width -1 height 1 opaque
p 'output7.txt' u 1:2 w p pointtype 7 \
        title 'Given data points', \
        'output7.txt' u 1:3 w l title 'Fitted curve'

# In terminal, run
# ~$ 'gnuplot -persist gnuplot_script7.gnu'
# Without the quotes. Plot will be displayed if gnuplot is installed
