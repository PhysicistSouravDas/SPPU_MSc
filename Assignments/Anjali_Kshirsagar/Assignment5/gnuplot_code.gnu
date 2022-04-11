set terminal 'qt'
set xzeroaxis
set yzeroaxis
set arrow from -5/0.529,0 to -5.0/0.529,1 nohead lc rgb 'red'
set arrow from 5/0.529,0 to 5.0/0.529,1 nohead lc rgb 'red'
set xlabel "x (bohr unit)"
set ylabel "u(x)"
set title "Wave function plot, a = 5 angstrom, V_0 = 5 eV"
set key box width 0 height 1 opaque
set xtics (-40, -30, -20, -9.45, 0, 9.45, 20, 30, 40)
set label "Region I" at -32,0.5
set label "Region II" at -2,0.5
set label "Region III" at 28,0.5
p 'non_normalized_wave.txt' u 1:2 w l lc rgb 'blue' lw 2 title \
    "Non-normalized wave function", 'normalized_wave.txt' u 1:2 w l \
    lc rgb 'green' lw 2 title "Normalized wave function"
# to execute this file and plot, type 
# 'gnuplot -persist gnuplot_code.gnu'
