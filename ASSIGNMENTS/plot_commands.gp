 set title 'Particle Motion: Height vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Elastic_And_Damping.png'
 plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'
 set title 'Particle Motion: Height vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Elastic.png'
 plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'
 set title 'Particle Motion: F_net vs. Height'
 set xlabel 'Height (m)'
 set ylabel 'F_net (N)'
 set grid
 set terminal png size 800,600
 set output 'F_net_vs_Height.png'
 plot 'results_damping_F_net.txt' using 2:6 with lines title 'Net_Force (F_net)'
 set title 'Particle Motion: F_contact vs. Height'
 set xlabel 'Height (m)'
 set ylabel 'F_contact (N)'
 set grid
 set terminal png size 800,600
 set output 'F_contact_vs_Height.png'
 plot 'results_damping_F_contact.txt' using 2:5 with lines title 'Contact_Force (F_contact)'
 set title 'Particle Motion: F_g vs. Height'
 set xlabel 'Height (m)'
 set ylabel 'F_g (N)'
 set grid
 set terminal png size 800,600
 set output 'F_g_vs_Height.png'
 plot 'results_damping_F_g.txt' using 2:4 with lines title 'Gravitational_Force (F_g)'
 set title 'Particle Motion: F_net vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'F_net (N)'
 set grid
 set terminal png size 800,600
 set output 'F_net_vs_Time.png'
 plot 'results_damping_F_net_vs_Time.txt' using 1:6 with lines title 'Net_Force (F_net)'
 set title 'Particle Motion: F_contact vs. Time'
 set xlabel 'Time (s)'
 set ylabel 'F_contact (N)'
 set grid
 set terminal png size 800,600
 set output 'F_contact_vs_Time.png'
 plot 'results_damping_F_contact_vs_Time.txt' using 1:5 with lines title 'Contact_Force (F_contact)'
