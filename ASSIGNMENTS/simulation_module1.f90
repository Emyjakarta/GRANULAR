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

    ! Save the plot as JPEG
    write(20, *) "set terminal jpeg size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic_And_Damping.jpeg'"
    write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'"

    ! Save the plot as GIF
    write(20, *) "set terminal gif size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic_And_Damping.gif'"
    write(20, *) "plot 'results_damping.txt' using 1:2 with lines title 'Height (z)'"

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

    ! Save the plot as JPEG
    write(20, *) "set terminal jpeg size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic.jpeg'"
    write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    ! Save the plot as GIF
    write(20, *) "set terminal gif size 800,600"
    write(20, *) "set output 'height_vs_time_Elastic.gif'"
    write(20, *) "plot 'results_elastic.txt' using 1:2 with lines title 'Height (z)'"

    
    close(20)

    ! Execute the Gnuplot script
    call execute_command_line("gnuplot plot_commands.gp")
end subroutine generate_plot


end module simulation_module

