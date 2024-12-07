 set title 'Particle Motion: Height vs. Time (1D Damping Case)'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Damping.png'
 plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'
 set title 'Delta vs Time (1D Damped Case)'
 set xlabel 'Time (s)'
 set ylabel 'Delta (m)'
 set grid
 set terminal png size 800,600
 set output 'delta_vs_time_1D_Damped.png'
 plot 'results_damping.txt' using 1:7 with lines title '1D Damped Delta'
 set title '1D Energy vs Time (Damping)'
 set xlabel 'Time (s)'
 set ylabel 'Energy (J)'
 set grid
 set terminal png size 800,600
 set output 'energy_vs_time_1D_damping.png'
 plot 'results_damping.txt' using 1:8 with lines title '1D Damping Energy'
 set title 'Particle Motion: Height vs. Time (1D Elastic Case)'
 set xlabel 'Time (s)'
 set ylabel 'Height (m)'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_Elastic.png'
 plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'
 set title 'Delta vs Time (1D Elastic Case)'
 set xlabel 'Time (s)'
 set ylabel 'Delta (m)'
 set grid
 set terminal png size 800,600
 set output 'delta_vs_time_1D_Elastic.png'
 plot 'results_elastic.txt' using 1:7 with lines title '1D Elastic Delta'
 set title 'Particle Motion: F_net vs. Time (1D Elastic Case)'
 set xlabel 'Time (s)'
 set ylabel 'F_net (N)'
 set grid
 set terminal png size 800,600
 set output 'F_net_vs_Time_elastic.png'
 plot 'results_elastic.txt' using 1:6 with lines title 'Net_Force Elastic (F_net)'
 set title 'Particle Motion: F_contact vs. Time (1D Elastic Case)'
 set xlabel 'Time (s)'
 set ylabel 'F_contact (N)'
 set grid
 set terminal png size 800,600
 set output 'F_contact_vs_Time_elastic.png'
 plot 'results_elastic.txt' using 1:5 with lines title 'Contact_Force Elastic (F_contact)'
 set title 'Force vs Delta (1D Elastic Case)'
 set xlabel 'Delta (m)'
 set ylabel 'Contact Force (N)'
 set grid
 set terminal png size 800,600
 set output 'force_vs_delta_1D_Elastic.png'
 plot 'results_elastic.txt' using 7:6 with lines title '1D Elastic Force vs Delta'
 set title '1D Velocity vs Time (Elastic and Damped)'
 set xlabel 'Time (s)'
 set ylabel 'Velocity (m/s)'
 set grid
 set terminal png size 800,600
 set output 'velocity_vs_time_1D.png'
 plot 'results_elastic.txt' using 1:3 with lines title '1D Elastic Velocity', 'results_damping.txt' using 1:3 with lines title '1D Damped Velocity'
 set title '1D Energy vs Time (Elastic)'
 set xlabel 'Time (s)'
 set ylabel 'Energy (J)'
 set grid
 set terminal png size 800,600
 set output 'energy_vs_time_1D_elastic.png'
 plot 'results_elastic.txt' using 1:8 with lines title '1D Elastic Energy'
 set title 'Particle Motion: F_net vs. Time (1D Damping Case)'
 set xlabel 'Time (s)'
 set ylabel 'F_net (N)'
 set grid
 set terminal png size 800,600
 set output 'F_net_vs_Time_damping.png'
 plot 'results_damping.txt' using 1:6 with lines title 'Net_Force Damping (F_net)'
 set title 'Particle Motion: F_contact vs. Time (1D Damping Case)'
 set xlabel 'Time (s)'
 set ylabel 'F_contact (N)'
 set grid
 set terminal png size 800,600
 set output 'F_contact_vs_Time_elastic.png'
 plot 'results_damping.txt' using 1:5 with lines title 'Contact_Force Damping (F_contact)'
 set title 'Force vs Delta (1D Damping Case)'
 set xlabel 'Delta (m)'
 set ylabel 'Contact Force (N)'
 set grid
 set terminal png size 800,600
 set output 'force_vs_delta_1D_Damping.png'
 plot 'results_damping.txt' using 7:6 with lines title '1D Damping Force vs Delta'
 set title 'Delta vs Time (2D Case)'
 set xlabel 'Time (s)'
 set ylabel 'Delta (m)'
 set grid
 set terminal png size 800,600
 set output 'delta_vs_time_2D.png'
 plot 'results_2D.txt' using 1:8 with lines title '2D Delta'
 set title '2D Particle Trajectory: Y vs X (2D Case)'
 set xlabel 'X (m)'
 set ylabel 'Y (m)'
 set grid
 set terminal png size 800,600
 set output 'trajectory_2D.png'
 plot 'results_2D.txt' using 2:3 with lines title 'Trajectory'
 set title 'Velocity Components vs Time (2D Case)'
 set xlabel 'Time (s)'
 set ylabel 'Velocity (m/s)'
 set grid
 set terminal png size 800,600
 set output 'velocity_vs_time.png'
 plot 'results_2D.txt' using 1:4 with lines title 'Vx', '' using 1:5 with lines title 'Vy'
 set title 'Force Components vs Time (2D Case)'
 set xlabel 'Time (s)'
 set ylabel 'Force (N)'
 set grid
 set terminal png size 800,600
 set output 'forces_vs_time.png'
 plot 'results_2D.txt' using 1:6 with lines title 'Fx', '' using 1:7 with lines title 'Fy'
 set title 'Force vs Delta (2D Case)'
 set xlabel 'Delta (m)'
 set ylabel 'Contact Force (N)'
 set grid
 set terminal png size 800,600
 set output 'force_vs_delta_2D.png'
 plot 'results_2D.txt' using 8:6 with lines title '2D Force vs Delta'
 set title '2D Energy vs Time'
 set xlabel 'Time (s)'
 set ylabel 'Energy (J)'
 set grid
 set terminal png size 800,600
 set output 'energy_vs_time_2D.png'
 plot 'results_2D.txt' using 1:9 with lines title '2D Energy'
