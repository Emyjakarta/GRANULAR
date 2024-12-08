module simulation_module ! Module for simulation subroutines
  implicit none ! Declare all variables explicitly
  contains ! Contains the subroutines for the simulation 

  ! Subroutine to initialize the simulation in 1D
  subroutine initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
    implicit none ! Declare all variables explicitly 
    real, intent(out) :: z(:), v(:), time_array(:) ! Declare all variables explicitly. Real means floating point numbers in single precision
    real, intent(in) :: z0, v0, dt
    integer, intent(in) :: n_points ! Integer means whole numbers without decimal points 
    integer :: i

    ! Initialize arrays with initial values
    z(1) = z0
    v(1) = v0
    do i = 1, n_points
      time_array(i) = (i - 1) * dt
    end do
  end subroutine initialize_simulation ! End of the subroutine 

  ! Subroutine to compute the forces
  subroutine compute_forces(z, v, m, g, k, c, F_g, F_contact, F_net, include_damping) 
    implicit none
    real, intent(in) :: z, v, m, g, k, c
    logical, intent(in), optional :: include_damping
    real, intent(out) :: F_g, F_contact, F_net

    ! Gravitational force
    F_g = -m * g ! Negative sign indicates downward force

    ! Contact force
    if (z <= 0.0) then ! Particle is in contact with the ground
      if (present(include_damping) .and. include_damping) then  ! Check if damping is included
        F_contact = -k * z - c * v ! Elastic and damping forces combined
      else
        F_contact = -k * z ! Elastic force only (no damping)
      end if
    else
      F_contact = 0.0 ! No contact force if particle is above the ground
    end if

    ! Net force
    F_net = F_g + F_contact ! Sum of gravitational and contact forces
  end subroutine compute_forces

  ! Subroutine to run the simulation
  subroutine run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_elastic_array, delta_damped_array, R, energy_array)  
    implicit none
    real, intent(inout) :: z(:), v(:), time_array(:), energy_array(:) ! time_array used for time steps and energy_array for energy calculations
    real, intent(out) :: F_g_array(:), F_contact_array(:), F_net_array(:), delta_elastic_array(:), delta_damped_array(:)
    real, intent(in) :: m, g, k, c, dt, epsilon, R ! m is mass, g is gravity, k is spring constant, c is damping coefficient, dt is time step, epsilon is stopping threshold, R is radius
    logical, intent(in) :: include_damping ! Logical means true or false
    integer, intent(in) :: n_points ! n_points is the number of points in the simulation 
    real :: F_g, F_contact, F_net, a, delta_elastic, delta_damped, initial_energy, current_energy ! delta_elastic and delta_damped are used for calculating the delta in elastic and damping cases
    integer :: i

    ! Initialize energy calculations
    initial_energy = 0.5 * m * v(1)**2 + m * g * z(1) ! Initial total energy
    energy_array(1) = initial_energy ! Store initial energy in the array

    do i = 2, n_points
      ! Compute forces
      call compute_forces(z(i - 1), v(i - 1), m, g, k, c, F_g, F_contact, F_net, include_damping) ! Call the subroutine to compute forces

      ! Store forces in arrays
      F_g_array(i) = F_g ! Store gravitational force
      F_contact_array(i) = F_contact ! Store contact force
      F_net_array(i) = F_net  ! Store net force


      ! Compute acceleration
      a = F_net / m

      ! Update velocity and position
      v(i) = v(i - 1) + a * dt ! Update velocity based on acceleration
      z(i) = z(i - 1) + v(i) * dt

      ! Compute delta for 1D Elastic Case
      if (.not. include_damping) then ! Check if damping is not included
        if (z(i) >= R) then ! Check if the particle is above the radius
          delta_elastic = 0.0 ! No deformation if above the radius
        else ! If the particle is below the radius
          delta_elastic = max(0.0, R - z(i)) ! Based on height difference from the radius
        end if 
        delta_elastic_array(i) = delta_elastic ! Store the delta in the array
      end if 

      ! Compute delta for 1D Damping Case
      if (include_damping) then  ! Check if damping is included
        delta_damped = abs(z(i)) * 0.5 ! scaled by height of the particle
        delta_damped_array(i) = delta_damped ! Store the delta in the array
      end if

      ! Compute current energy after updating velocity and position
      current_energy = 0.5 * m * v(i)**2 + m * g * z(i) ! Calculate current total energy
      energy_array(i) = current_energy ! Store current energy in the array
      ! Stop if velocity and height are small
      if (abs(v(i)) < epsilon .and. z(i) == 0.0) then ! Check if velocity is small and height is zero
        z(i:) = 0.0 ! Set height to zero for remaining points
        v(i:) = 0.0 ! Set velocity to zero for remaining points
        F_g_array(i:) = 0.0 ! Set gravitational force to zero for remaining points
        F_contact_array(i:) = 0.0 ! Set contact force to zero for remaining points
        F_net_array(i:) = 0.0 ! Set net force to zero for remaining points
        delta_elastic_array(i:) = 0.0 ! Set delta for elastic case to zero for remaining points
        delta_damped_array(i:) = 0.0 ! Set delta for damping case to zero for remaining points
        exit ! Exit the loop
      end if 
    end do
  end subroutine run_simulation

  ! Subroutine to write results to a file for 1D simulation
  subroutine write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, filename, energy_array)  
    implicit none
    real, intent(in) :: z(:), v(:), time_array(:)
    real, intent(in) :: F_g_array(:), F_contact_array(:), F_net_array(:)
    real, intent(in) :: delta_array(:), energy_array(:)
    integer, intent(in) :: n_points
    character(len=*), intent(in) :: filename
    integer :: i, j, block_size
    real :: avg_time, avg_z, avg_v, avg_Fg, avg_Fcontact, avg_Fnet, avg_delta, avg_energy

    block_size = 100 ! Block size for averaging (every 100 points) 

    ! Open the file for writing results
    open(unit=10, file=filename, status="replace") 
    
    ! Write the header line, including "Delta (m)" and "Energy (J)"
    write(10, '(A)') "Time (s), Height (m), Velocity (m/s), Gravitational Force (N), Contact Force (N), Net Force (N), Delta (m), Energy (J)" 
    
    ! Write every 100th term or block average to the file
    do i = 1, n_points, block_size ! Loop over the points with block size
      if (i + block_size - 1 <= n_points) then ! Check if block size is within the total points
        ! Calculate averages for the block size 
        avg_time = sum(time_array(i:i + block_size - 1)) / block_size 
        avg_z = sum(z(i:i + block_size - 1)) / block_size
        avg_v = sum(v(i:i + block_size - 1)) / block_size
        avg_Fg = sum(F_g_array(i:i + block_size - 1)) / block_size
        avg_Fcontact = sum(F_contact_array(i:i + block_size - 1)) / block_size
        avg_Fnet = sum(F_net_array(i:i + block_size - 1)) / block_size
        avg_delta = sum(delta_array(i:i + block_size - 1)) / block_size
        avg_energy = sum(energy_array(i:i + block_size - 1)) / block_size

        ! Write averages to the file (with 3 decimal places) Taking the averages after every 100 points 
        if (time_array(i) <= 0.05) then ! Check if time is less than 0.05 seconds 
          write(10, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') avg_time, avg_z, avg_v, avg_Fg, avg_Fcontact, avg_Fnet, avg_delta, avg_energy
        else
          write(10, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') avg_time, avg_z, avg_v, avg_Fg, avg_Fcontact, avg_Fnet, avg_delta, avg_energy 
        end if 
      end if
    end do
    close(10) ! Close the file after writing results. 10 is the unit number. unit number is used to identify the file in the program
  end subroutine write_results

  subroutine generate_plot() ! Subroutine to generate plots  for 1D and 2D cases
    implicit none
    open(unit=20, file="plot_commands.gp", status="replace") ! Open the file for writing plot commands
    
    ! For Damping Case 1D  (Height vs. Time)
    ! Set up Gnuplot for plotting and saving the output. Gnuplot is a command-line program that can generate two- and three-dimensional plots of functions, data, and data fits.
    write(20, *) "set title 'Particle Motion: Height vs. Time (1D Damping Case)'" ! Set the title of the plot
    write(20, *) "set xlabel 'Time (s)'" ! Set the label for the x-axis
    write(20, *) "set ylabel 'Height (m)'" ! Set the label for the y-axis
    write(20, *) "set grid" ! Set the grid for the plot
    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600" ! Set the terminal for saving the plot as PNG and the resolution size
    write(20, *) "set output 'height_vs_time_Damping.png'" ! Set the output file name
    write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'" ! Plot the height vs. time

    ! 1D Damped Case (Delta vs. Time)
    write(20, *) "set title 'Delta vs Time (1D Damped Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Delta (m)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'delta_vs_time_1D_Damped.png'"
    write(20, *) "plot 'results_damping.txt' using 1:7 with lines title '1D Damped Delta'"

    ! For Damping Case (Energy vs. Time) 1D
    write(20, *) "set title '1D Energy vs Time (Damping)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Energy (J)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'energy_vs_time_1D_damping.png'"
    write(20, *) "plot 'results_damping.txt' using 1:8 with lines title '1D Damping Energy'"

    ! For Elastic with No Damping Case 1D (Height vs. Time)
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: Height vs. Time (1D Elastic Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Height (m)'"
    write(20, *) "set grid"

    ! Save the plot as PNG 
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    ! 1D Elastic Case (Delta vs. Time)
    write(20, *) "set title 'Delta vs Time (1D Elastic Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Delta (m)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'delta_vs_time_1D_Elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:7 with lines title '1D Elastic Delta'"

    ! For Elastic Case (Net Force vs. Time) 1D 
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_net vs. Time (1D Elastic Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_net (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_net_vs_Time_elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:6 with lines title 'Net_Force Elastic (F_net)'"

    ! For Elastic Case (Contact Force vs. Time) 1D
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_contact vs. Time (1D Elastic Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_contact (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_contact_vs_Time_elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:5 with lines title 'Contact_Force Elastic (F_contact)'"

    ! For 1D Elastic Case: Force vs. Delta 
    write(20, *) "set title 'Force vs Delta (1D Elastic Case)'"
    write(20, *) "set xlabel 'Delta (m)'"
    write(20, *) "set ylabel 'Net Force (N)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'force_vs_delta_1D_Elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 7:6 with lines title '1D Elastic Force vs Delta'"

    ! Plot velocity vs time for 1D cases (Elastic and Damped)
    write(20, *) "set title '1D Velocity vs Time (Elastic and Damped)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Velocity (m/s)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'velocity_vs_time_1D.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:3 with lines title '1D Elastic Velocity', 'results_damping.txt' using 1:3 with lines title '1D Damped Velocity'"

    ! For Elastic Case (Energy vs. Time) 1D 
    write(20, *) "set title '1D Energy vs Time (Elastic)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Energy (J)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'energy_vs_time_1D_elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:8 with lines title '1D Elastic Energy'"

    ! For Damping Case (Net Force vs. Time) 1D
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_net vs. Time (1D Damping Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_net (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_net_vs_Time_damping.png'"
    write(20, *) "plot 'results_damping.txt' using 1:6 with lines title 'Net_Force Damping (F_net)'"

    ! For Damping Case (Contact Force vs. Time) 1D
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_contact vs. Time (1D Damping Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_contact (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_contact_vs_Time_elastic.png'"
    write(20, *) "plot 'results_damping.txt' using 1:5 with lines title 'Contact_Force Damping (F_contact)'"

    ! For 1D Damping Case: Force vs. Delta 
    write(20, *) "set title 'Force vs Delta (1D Damping Case)'"
    write(20, *) "set xlabel 'Delta (m)'"
    write(20, *) "set ylabel 'Net Force (N)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'force_vs_delta_1D_Damping.png'"
    write(20, *) "plot 'results_damping.txt' using 7:6 with lines title '1D Damping Force vs Delta'"

    ! 2D Case (Delta vs. Time)
    write(20, *) "set title 'Delta vs Time (2D Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Delta (m)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'delta_vs_time_2D.png'"
    write(20, *) "plot 'results_2D.txt' using 1:8 with lines title '2D Delta'"

    
    ! Y vs X Plot 2D 
    write(20, *) "set title '2D Particle Trajectory: Y vs X (2D Case)'"
    write(20, *) "set xlabel 'X (m)'"
    write(20, *) "set ylabel 'Y (m)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'trajectory_2D.png'"
    write(20, *) "plot 'results_2D.txt' using 2:3 with lines title 'Trajectory'"

    ! Velocity Components vs Time Plot 2D 
    write(20, *) "set title 'Velocity Components vs Time (2D Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Velocity (m/s)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'velocity_vs_time.png'"
    write(20, *) "plot 'results_2D.txt' using 1:4 with lines title 'Vx', '' using 1:5 with lines title 'Vy'"

    ! Forces vs Time Plot 2D 
    write(20, *) "set title 'Force Components vs Time (2D Case)'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Force (N)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'forces_vs_time.png'"
    write(20, *) "plot 'results_2D.txt' using 1:6 with lines title 'Fx', '' using 1:7 with lines title 'Fy'"

    ! For 2D Case: Force vs. Delta 
    write(20, *) "set title 'Force vs Delta (2D Case)'"
    write(20, *) "set xlabel 'Delta (m)'"
    write(20, *) "set ylabel 'Force (N)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'force_vs_delta_2D.png'"
    write(20, *) "plot 'results_2D.txt' using 8:6 with lines title '2D Force Fx vs Delta', '' using 8:7 with lines title '2D Force Fy vs Delta'"

    ! Energy vs Time Plot 2D 
    write(20, *) "set title '2D Energy vs Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Energy (J)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'energy_vs_time_2D.png'"
    write(20, *) "plot 'results_2D.txt' using 1:9 with lines title '2D Energy'"
    
    close(20)

    ! Execute the Gnuplot script 
    call execute_command_line("gnuplot plot_commands.gp") ! Execute_command_line is a subroutine that executes a command line in the terminal. Here, it executes the Gnuplot script
end subroutine generate_plot

 ! Subroutine to initialize the 2D simulation
subroutine initialize_simulation_2D(x, y, vx, vy, time_array, x0, y0, vx0, vy0, dt, n_points)
  implicit none
  real, intent(out) :: x(:), y(:), vx(:), vy(:), time_array(:)
  real, intent(in) :: x0, y0, vx0, vy0, dt
  integer, intent(in) :: n_points ! Number of points in the simulation
  integer :: i

  ! Initialize arrays
  x(1) = x0
  y(1) = y0
  vx(1) = vx0
  vy(1) = vy0
  do i = 1, n_points
    time_array(i) = (i - 1) * dt
  end do
end subroutine initialize_simulation_2D

! Subroutine to compute the forces in 2D
subroutine compute_forces_2D(x, y, vx, vy, m, g, k, c, mu, Fx, Fy, include_damping)
  implicit none
  real, intent(in) :: x, y, vx, vy, m, g, k, c, mu
  logical, intent(in), optional :: include_damping
  real, intent(out) :: Fx, Fy

  real :: delta, Fn, Ft

  ! Normal force (contact with wall at y=0) and friction force
  if (y <= 0.0) then ! Particle is in contact with the wall
    delta = -y  ! Penetration depth (positive value)
    Fn = k * delta  ! Normal force
    if (present(include_damping) .and. include_damping) then  ! Check if damping is included
      Fn = Fn - c * vy  ! Damping force
    end if  
    ! Friction force
    Ft = mu * Fn
    Fx = -Ft * sign(1.0, vx)  ! Friction opposes motion in x direction
    Fy = Fn  ! Normal force in y direction
  else
    Fx = 0.0 ! No forces if particle is not in contact with the wall
    Fy = -m * g  ! Gravity acts in y direction (negative)
  end if 
end subroutine compute_forces_2D 

! Subroutine to run the 2D simulation
subroutine run_simulation_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, m, g, k, c, mu, dt, epsilon, n_points, include_damping, delta_2D_array, energy_array) 
  implicit none
  real, intent(inout) :: x(:), y(:), vx(:), vy(:), time_array(:), energy_array(:) 
  real, intent(out) :: Fx_array(:), Fy_array(:), delta_2D_array(:)
  real, intent(in) :: m, g, k, c, mu, dt, epsilon
  logical, intent(in) :: include_damping ! Logical means true or false. Check if damping is included
  integer, intent(in) :: n_points ! Number of points in the simulation
  real :: Fx, Fy, ax, ay, initial_energy, current_energy, delta_2D
  integer :: i

  ! Initialize energy calculations
  initial_energy = 0.5 * m * (vx(1)**2 + vy(1)**2) + m * g * y(1) ! Initial total energy
  energy_array(1) = initial_energy

  do i = 2, n_points
    ! Compute forces
    call compute_forces_2D(x(i-1), y(i-1), vx(i-1), vy(i-1), m, g, k, c, mu, Fx, Fy, include_damping)

    ! Store forces
    Fx_array(i) = Fx
    Fy_array(i) = Fy

    ! Compute accelerations
    ax = Fx / m
    ay = Fy / m

    ! Update velocities
    vx(i) = vx(i-1) + ax * dt
    vy(i) = vy(i-1) + ay * dt

    ! Update positions
    x(i) = x(i-1) + vx(i) * dt
    y(i) = y(i-1) + vy(i) * dt

    ! Calculate delta based on penetration depth 2D case
    if (y(i) < 0.0) then ! Particle is in contact with the wall
      delta_2D = -y(i)  ! Penetration depth (positive value) if below the wall
    else
      delta_2D = 0.0    ! No penetration if above the wall
    end if 
    delta_2D_array(i) = delta_2D ! Store the delta in the array

    ! Compute current energy and store it
    current_energy = 0.5 * m * (vx(i)**2 + vy(i)**2) + m * g * y(i)
    energy_array(i) = current_energy

    ! Stop if velocity and height are small
    if (abs(vy(i)) < epsilon .and. y(i) == 0.0) then ! Check if velocity is small and height is zero
      x(i:) = x(i-1) ! Set x position to the last value
      y(i:) = 0.0   ! Set y position to zero
      vx(i:) = 0.0 ! Set x velocity to zero
      vy(i:) = 0.0 ! Set y velocity to zero
      Fx_array(i:) = 0.0 ! Set x force to zero
      Fy_array(i:) = 0.0 ! Set y force to zero
      delta_2D_array(i:) = 0.0 ! Set delta to zero
      exit ! Exit the loop
    end if
  end do
end subroutine run_simulation_2D 

subroutine write_results_2D(x, y, vx, vy, Fx_array, Fy_array, delta_array, time_array, n_points, filename, energy_array)
  implicit none
  real, intent(in) :: x(:), y(:), vx(:), vy(:), Fx_array(:), Fy_array(:), time_array(:), delta_array(:), energy_array(:)
  integer, intent(in) :: n_points
  character(len=*), intent(in) :: filename
  integer :: i, j, block_size
  real :: avg_time, avg_x, avg_y, avg_vx, avg_vy, avg_Fx, avg_Fy, avg_delta, avg_energy

  block_size = 100 ! Block size for averaging (every 100 points)
  
  ! Open the file for writing
  open(unit=20, file=filename, status="replace")
  ! Write the header line
  write(20, '(A)') "Time (s), X (m), Y (m), Vx (m/s), Vy (m/s), Fx (N), Fy (N), Delta (m), Energy (J)"
  ! Write every 100th term or block average
  do i = 1, n_points, block_size
    if (i + block_size - 1 <= n_points) then
      ! Calculate averages for the block
      avg_time = sum(time_array(i:i + block_size - 1)) / block_size
      avg_x = sum(x(i:i + block_size - 1)) / block_size
      avg_y = sum(y(i:i + block_size - 1)) / block_size
      avg_vx = sum(vx(i:i + block_size - 1)) / block_size
      avg_vy = sum(vy(i:i + block_size - 1)) / block_size
      avg_Fx = sum(Fx_array(i:i + block_size - 1)) / block_size
      avg_Fy = sum(Fy_array(i:i + block_size - 1)) / block_size
      avg_delta = sum(delta_array(i:i + block_size - 1)) / block_size
      avg_energy = sum(energy_array(i:i + block_size - 1)) / block_size

      ! Write averages to the file
      if (time_array(i) <= 0.05) then
        write(20, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') avg_time, avg_x, avg_y, avg_vx, avg_vy, avg_Fx, avg_Fy, avg_delta, avg_energy  
      else
        write(20, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') avg_time, avg_x, avg_y, avg_vx, avg_vy, avg_Fx, avg_Fy, avg_delta, avg_energy
      end if
    end if
  end do
  close(20)
end subroutine write_results_2D

end module simulation_module