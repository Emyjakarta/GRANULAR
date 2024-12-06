module simulation_module
  implicit none
  contains

  ! Subroutine to initialize the simulation
  subroutine initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
    implicit none
    real, intent(out) :: z(:), v(:), time_array(:)
    real, intent(in) :: z0, v0, dt
    integer, intent(in) :: n_points
    integer :: i

    ! Initialize arrays
    z(1) = z0
    v(1) = v0
    do i = 1, n_points
      time_array(i) = (i - 1) * dt
    end do
  end subroutine initialize_simulation

  ! Subroutine to compute the forces
  subroutine compute_forces(z, v, m, g, k, c, F_g, F_contact, F_net, include_damping)
    implicit none
    real, intent(in) :: z, v, m, g, k, c
    logical, intent(in), optional :: include_damping
    real, intent(out) :: F_g, F_contact, F_net

    ! Gravitational force
    F_g = -m * g

    ! Contact force
    if (z <= 0.0) then
      if (present(include_damping) .and. include_damping) then
        F_contact = -k * z - c * v ! Elastic and damping forces
      else
        F_contact = -k * z ! Elastic force only
      end if
      ! F_contact = -k * z - c * v
    else
      F_contact = 0.0
    end if

    ! Net force
    F_net = F_g + F_contact
  end subroutine compute_forces

  ! Subroutine to run the simulation
  subroutine run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping)
    implicit none
    real, intent(inout) :: z(:), v(:), time_array(:)
    real, intent(out) :: F_g_array(:), F_contact_array(:), F_net_array(:)
    real, intent(in) :: m, g, k, c, dt, epsilon
    logical, intent(in) :: include_damping
    integer, intent(in) :: n_points
    real :: F_g, F_contact, F_net, a
    integer :: i

    do i = 2, n_points
      ! Compute forces
      call compute_forces(z(i - 1), v(i - 1), m, g, k, c, F_g, F_contact, F_net, include_damping)

      ! Store forces in arrays
      F_g_array(i) = F_g
      F_contact_array(i) = F_contact
      F_net_array(i) = F_net


      ! Compute acceleration
      a = F_net / m

      ! Update velocity and position
      v(i) = v(i - 1) + a * dt
      z(i) = z(i - 1) + v(i) * dt

      ! Stop if velocity and height are small
      if (abs(v(i)) < epsilon .and. z(i) == 0.0) then
        z(i:) = 0.0
        v(i:) = 0.0
        F_g_array(i:) = 0.0
        F_contact_array(i:) = 0.0
        F_net_array(i:) = 0.0
        exit
      end if
    end do
  end subroutine run_simulation

  ! Subroutine to write results to a file
  ! subroutine write_results(z, v, time_array, n_points)
  subroutine write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, n_points, filename)
    implicit none
    real, intent(in) :: z(:), v(:), time_array(:)
    real, intent(in) :: F_g_array(:), F_contact_array(:), F_net_array(:)
    integer, intent(in) :: n_points
    character(len=*), intent(in) :: filename
    integer :: i
    open(unit=10, file=filename, status="replace")
    ! write(10, *) "Time (s)", "Height (m)", "Velocity (m/s)", "Gravitational_Force", "Contact_Force", "Net_Force"
    write(10, '(A)') "Time (s), Height (m), Velocity (m/s), Gravitational Force (N), Contact Force (N), Net Force (N)"
    do i = 1, n_points
      ! write(10, *) time_array(i), z(i), v(i)
      write(10, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') time_array(i), z(i), v(i), F_g_array(i), F_contact_array(i), F_net_array(i)
    end do
    close(10)
  end subroutine write_results

  ! Subroutine to generate plot using Gnuplot
  ! subroutine generate_plot()
  !   implicit none
  !   open(unit=20, file="plot_commands.gp", status="replace")
  !   write(20, *) "set title 'Particle Motion: Height vs. Time'"
  !   write(20, *) "set xlabel 'Time (s)'"
  !   write(20, *) "set ylabel 'Height (m)'"
  !   write(20, *) "set grid"
  !   write(20, *) "plot 'results.txt' using 1:2 with lines title 'Height (z)'"
  !   write(20, *) "pause -1 'Press Enter to continue...'"
  !   close(20)
  !   call execute_command_line("gnuplot -persist plot_commands.gp")
  ! end subroutine generate_plot

  subroutine generate_plot()
    implicit none
    open(unit=20, file="plot_commands.gp", status="replace")
    
    ! For Elastic and Damping Case
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: Height vs. Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Height (m)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic_And_Damping.png'"
    write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'"

    ! ! Save the plot as JPEG
    ! write(20, *) "set terminal jpeg size 800,600"
    ! write(20, *) "set output 'height_vs_time_Elastic_And_Damping.jpeg'"
    ! write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'"

    ! ! Save the plot as GIF
    ! write(20, *) "set terminal gif size 800,600"
    ! write(20, *) "set output 'height_vs_time_Elastic_And_Damping.gif'"
    ! write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'"

    ! ! Display the plot in Gnuplot (optional)
    ! write(20, *) "set terminal qt"
    ! write(20, *) "unset output"
    ! write(20, *) "plot 'results.txt' using 1:2 with lines title 'Height (z)'"

    ! For Elastic with No Damping Case
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: Height vs. Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Height (m)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic.png'"
    write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    ! ! Save the plot as JPEG
    ! write(20, *) "set terminal jpeg size 800,600"
    ! write(20, *) "set output 'height_vs_time_Elastic.jpeg'"
    ! write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    ! ! Save the plot as GIF
    ! write(20, *) "set terminal gif size 800,600"
    ! write(20, *) "set output 'height_vs_time_Elastic.gif'"
    ! write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    ! ! For Elastic and Damping Case (Net Force)
    ! ! Set up Gnuplot for plotting and saving the output
    ! write(20, *) "set title 'Particle Motion: F_net vs. Height'"
    ! write(20, *) "set xlabel 'Height (m)'"
    ! write(20, *) "set ylabel 'F_net (N)'"
    ! write(20, *) "set grid"

    ! ! Save the plot as PNG
    ! write(20, *) "set terminal png size 800,600"
    ! write(20, *) "set output 'F_net_vs_Height.png'"
    ! write(20, *) "plot 'results_damping_F_net.txt' using 2:6 with lines title 'Net_Force (F_net)'"

    ! ! For Elastic and Damping Case (Contact Force)
    ! ! Set up Gnuplot for plotting and saving the output
    ! write(20, *) "set title 'Particle Motion: F_contact vs. Height'"
    ! write(20, *) "set xlabel 'Height (m)'"
    ! write(20, *) "set ylabel 'F_contact (N)'"
    ! write(20, *) "set grid"

    ! ! Save the plot as PNG
    ! write(20, *) "set terminal png size 800,600"
    ! write(20, *) "set output 'F_contact_vs_Height.png'"
    ! write(20, *) "plot 'results_damping_F_contact.txt' using 2:5 with lines title 'Contact_Force (F_contact)'"

    ! ! For Elastic and Damping Case (Gravitational Force)
    ! ! Set up Gnuplot for plotting and saving the output
    ! write(20, *) "set title 'Particle Motion: F_g vs. Height'"
    ! write(20, *) "set xlabel 'Height (m)'"
    ! write(20, *) "set ylabel 'F_g (N)'"
    ! write(20, *) "set grid"

    ! ! Save the plot as PNG
    ! write(20, *) "set terminal png size 800,600"
    ! write(20, *) "set output 'F_g_vs_Height.png'"
    ! write(20, *) "plot 'results_damping_F_g.txt' using 2:4 with lines title 'Gravitational_Force (F_g)'"

    ! For Elastic and Damping Case (Net Force vs. Time)
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_net vs. Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_net (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_net_vs_Time.png'"
    write(20, *) "plot 'results_damping_F_net_vs_Time.txt' using 1:6 with lines title 'Net_Force (F_net)'"

    ! For Elastic and Damping Case (Contact Force vs. Time)
    ! Set up Gnuplot for plotting and saving the output
    write(20, *) "set title 'Particle Motion: F_contact vs. Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'F_contact (N)'"
    write(20, *) "set grid"

    ! Save the plot as PNG
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'F_contact_vs_Time.png'"
    write(20, *) "plot 'results_damping_F_contact_vs_Time.txt' using 1:5 with lines title 'Contact_Force (F_contact)'"

    ! Y vs X Plot
    write(20, *) "set title '2D Particle Trajectory: Y vs X'"
    write(20, *) "set xlabel 'X (m)'"
    write(20, *) "set ylabel 'Y (m)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'trajectory_2D.png'"
    write(20, *) "plot 'results_2D.txt' using 2:3 with lines title 'Trajectory'"

    ! Velocity Components vs Time Plot
    write(20, *) "set title 'Velocity Components vs Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Velocity (m/s)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'velocity_vs_time.png'"
    write(20, *) "plot 'results_2D.txt' using 1:4 with lines title 'Vx', '' using 1:5 with lines title 'Vy'"

    ! Forces vs Time Plot
    write(20, *) "set title 'Force Components vs Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Force (N)'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'forces_vs_time.png'"
    write(20, *) "plot 'results_2D.txt' using 1:6 with lines title 'Fx', '' using 1:7 with lines title 'Fy'"




    
    close(20)

    ! Execute the Gnuplot script
    call execute_command_line("gnuplot plot_commands.gp")
end subroutine generate_plot

 ! Subroutine to initialize the 2D simulation
subroutine initialize_simulation_2D(x, y, vx, vy, time_array, x0, y0, vx0, vy0, dt, n_points)
  implicit none
  real, intent(out) :: x(:), y(:), vx(:), vy(:), time_array(:)
  real, intent(in) :: x0, y0, vx0, vy0, dt
  integer, intent(in) :: n_points
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

  ! Normal force (contact with wall at y=0)
  if (y <= 0.0) then
    delta = -y
    Fn = k * delta
    if (present(include_damping) .and. include_damping) then
      Fn = Fn - c * vy
    end if
    ! Friction force
    Ft = mu * Fn
    Fx = -Ft * sign(1.0, vx)  ! Friction opposes motion in x
    Fy = Fn  ! Normal force
  else
    Fx = 0.0
    Fy = -m * g  ! Gravity acts in y direction
  end if
end subroutine compute_forces_2D

! Subroutine to run the 2D simulation
subroutine run_simulation_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, m, g, k, c, mu, dt, epsilon, n_points, include_damping)
  implicit none
  real, intent(inout) :: x(:), y(:), vx(:), vy(:), time_array(:)
  real, intent(out) :: Fx_array(:), Fy_array(:)
  real, intent(in) :: m, g, k, c, mu, dt, epsilon
  logical, intent(in) :: include_damping
  integer, intent(in) :: n_points
  real :: Fx, Fy, ax, ay
  integer :: i

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

    ! Stop if velocity and height are small
    if (abs(vy(i)) < epsilon .and. y(i) == 0.0) then
      x(i:) = x(i-1)
      y(i:) = 0.0
      vx(i:) = 0.0
      vy(i:) = 0.0
      Fx_array(i:) = 0.0
      Fy_array(i:) = 0.0
      exit
    end if
  end do
end subroutine run_simulation_2D

subroutine write_results_2D(x, y, vx, vy, Fx_array, Fy_array, time_array, n_points, filename)
  implicit none
  real, intent(in) :: x(:), y(:), vx(:), vy(:), Fx_array(:), Fy_array(:), time_array(:)
  integer, intent(in) :: n_points
  character(len=*), intent(in) :: filename
  integer :: i
  open(unit=20, file=filename, status="replace")
  write(20, '(A)') "Time (s), X (m), Y (m), Vx (m/s), Vy (m/s), Fx (N), Fy (N)"
  do i = 1, n_points
      write(20, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') &
          time_array(i), x(i), y(i), vx(i), vy(i), Fx_array(i), Fy_array(i)
  end do
  close(20)
end subroutine write_results_2D



end module simulation_module

