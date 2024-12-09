 set title 'Normalized Height vs. Time (Elastic Case)'
 set xlabel 'Time*'
 set ylabel 'Height*'
 set grid
 set terminal png size 800,600
 set output 'height_vs_time_elastic.png'
 plot 'results_normalized_1D.txt' using 1:2 with lines title 'Height* (z*)'
 set title 'Normalized Delta vs. Time (Elastic Case)'
 set xlabel 'Time*'
 set ylabel 'Delta*'
 set grid
 set terminal png size 800,600
 set output 'delta_vs_time_elastic.png'
 plot 'results_normalized_1D.txt' using 1:7 with lines title 'Delta* (Elastic)'
 set title 'Normalized Energy vs. Time (Elastic Case)'
 set xlabel 'Time*'
 set ylabel 'Energy*'
 set grid
 set terminal png size 800,600
 set output 'energy_vs_time_elastic.png'
 plot 'results_normalized_1D.txt' using 1:8 with lines title 'Energy* (Elastic)'
 set title 'Normalized Net Force vs. Time (Elastic Case)'
 set xlabel 'Time*'
 set ylabel 'Net Force*'
 set grid
 set terminal png size 800,600
 set output 'net_force_vs_time_elastic.png'
 plot 'results_normalized_1D.txt' using 1:6 with lines title 'Net Force* (Elastic)'
 set title 'Normalized Delta vs. Net Force (Elastic Case)'
 set xlabel 'Delta*'
 set ylabel 'Net Force*'
 set grid
 set terminal png size 800,600
 set output 'delta_vs_net_force_elastic.png'
 plot 'results_normalized_1D.txt' using 7:6 with lines title 'Delta* vs. Net Force*'
 set title 'Normalized 2D Trajectory (Y* vs X*)'
 set xlabel 'X*'
 set ylabel 'Y*'
 set grid
 set terminal png size 800,600
 set output 'trajectory_2D_normalized.png'
 plot 'results_normalized_2D.txt' using 2:3 with lines title 'Trajectory (Y* vs X*)'
 set title 'Normalized Velocity Components vs Time* (2D)'
 set xlabel 'Time*'
 set ylabel 'Velocity*'
 set grid
 set terminal png size 800,600
 set output 'velocity_components_2D_normalized.png'
 plot 'results_normalized_2D.txt' using 1:4 with lines title 'Vx*', '' using 1:5 with lines title 'Vy*'
 set title 'Normalized Forces vs Time* (2D)'
 set xlabel 'Time*'
 set ylabel 'Force*'
 set grid
 set terminal png size 800,600
 set output 'forces_vs_time_2D_normalized.png'
 plot 'results_normalized_2D.txt' using 1:6 with lines title 'Fx*', '' using 1:7 with lines title 'Fy*'
 set title 'Normalized Energy vs Time* (2D)'
 set xlabel 'Time*'
 set ylabel 'Energy*'
 set grid
 set terminal png size 800,600
 set output 'energy_vs_time_2D_normalized.png'
 plot 'results_normalized_2D.txt' using 1:9 with lines title 'Energy*'
