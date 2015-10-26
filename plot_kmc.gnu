set terminal postscript enhanced

set title "E1: X, E2: Y, E3: W; E4: Z"
set xlabel "Time (ps)"
set ylabel "Distance (arb. units)"

set output "kmc.eps"
plot 'kmc.out' u 1:2 w l t ""
