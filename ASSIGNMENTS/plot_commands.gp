 set title 'Particle Motion: Height vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Elastic_And_Damping.png'
 plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'
 set terminal jpeg size 800,600
 set output 'height_vs_time_Elastic_And_Damping.jpeg'
 plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'
 set terminal gif size 800,600
 set output 'height_vs_time_Elastic_And_Damping.gif'
 plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'
 set title 'Particle Motion: Height vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Elastic.png'
 plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'
 set terminal jpeg size 800,600
 set output 'height_vs_time_Elastic.jpeg'
 plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'
 set terminal gif size 800,600
 set output 'height_vs_time_Elastic.gif'
 plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'
