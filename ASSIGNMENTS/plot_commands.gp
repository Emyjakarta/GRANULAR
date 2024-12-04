 set title 'Particle Motion: Height vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 plot 'results.txt' using 1:2 with lines title 'Height (z)'
 pause -1 'Press Enter to continue...'
