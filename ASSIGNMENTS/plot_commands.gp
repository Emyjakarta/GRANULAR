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
 set title '2D Particle Trajectory: Y vs X'
 set xlabel 'X (m)'
 set ylabel 'Y (m)'
 set grid
 set terminal png size 800,600
 set output 'trajectory_2D.png'
 plot 'results_2D.txt' using 2:3 with lines title 'Trajectory'
 set title 'Velocity Components vs Time'
 set xlabel 'Time (s)'
 set ylabel 'Velocity (m/s)'
 set grid
 set terminal png size 800,600
 set output 'velocity_vs_time.png'
 plot 'results_2D.txt' using 1:4 with lines title 'Vx', '' using 1:5 with lines title 'Vy'
 set title 'Force Components vs Time'
 set xlabel 'Time (s)'
 set ylabel 'Force (N)'
 set grid
 set terminal png size 800,600
 set output 'forces_vs_time.png'
 plot 'results_2D.txt' using 1:6 with lines title 'Fx', '' using 1:7 with lines title 'Fy'
