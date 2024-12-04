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
  subroutine compute_forces(z, v, m, g, k, c, F_g, F_contact, F_net)
    implicit none
    real, intent(in) :: z, v, m, g, k, c
    real, intent(out) :: F_g, F_contact, F_net

    ! Gravitational force
    F_g = -m * g

    ! Contact force
    if (z <= 0.0) then
      F_contact = -k * z - c * v
    else
      F_contact = 0.0
    end if

    ! Net force
    F_net = F_g + F_contact
  end subroutine compute_forces

  ! Subroutine to run the simulation
  subroutine run_simulation(z, v, time_array, m, g, k, c, dt, epsilon, n_points)
    implicit none
    real, intent(inout) :: z(:), v(:), time_array(:)
    real, intent(in) :: m, g, k, c, dt, epsilon
    integer, intent(in) :: n_points
    real :: F_g, F_contact, F_net, a
    integer :: i

    do i = 2, n_points
      ! Compute forces
      call compute_forces(z(i - 1), v(i - 1), m, g, k, c, F_g, F_contact, F_net)

      ! Compute acceleration
      a = F_net / m

      ! Update velocity and position
      v(i) = v(i - 1) + a * dt
      z(i) = z(i - 1) + v(i) * dt

      ! Stop if velocity and height are small
      if (abs(v(i)) < epsilon .and. z(i) == 0.0) then
        z(i:) = 0.0
        v(i:) = 0.0
        exit
      end if
    end do
  end subroutine run_simulation

  ! Subroutine to write results to a file
  subroutine write_results(z, v, time_array, n_points)
    implicit none
    real, intent(in) :: z(:), v(:), time_array(:)
    integer, intent(in) :: n_points
    integer :: i
    open(unit=10, file="results.txt", status="replace")
    write(10, *) "Time (s)", "Height (m)", "Velocity (m/s)"
    do i = 1, n_points
      write(10, *) time_array(i), z(i), v(i)
    end do
    close(10)
  end subroutine write_results

  ! Subroutine to generate plot using Gnuplot
  subroutine generate_plot()
    implicit none
    open(unit=20, file="plot_commands.gp", status="replace")
    write(20, *) "set title 'Particle Motion: Height vs. Time'"
    write(20, *) "set xlabel 'Time (s)'"
    write(20, *) "set ylabel 'Height (m)'"
    write(20, *) "set grid"
    write(20, *) "plot 'results.txt' using 1:2 with lines title 'Height (z)'"
    write(20, *) "pause -1 'Press Enter to continue...'"
    close(20)
    call execute_command_line("gnuplot -persist plot_commands.gp")
  end subroutine generate_plot

end module simulation_module

