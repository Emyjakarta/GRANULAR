module simulation_module
  implicit none

  contains

  ! Subroutine to initialize the simulation in 1D using normalized variables
  subroutine initialize_simulation_normalized(z_star, v_star, time_star_array, z0, v0, dt_star, n_points)
    implicit none
    real, intent(out) :: z_star(:), v_star(:), time_star_array(:)
    real, intent(in) :: z0, v0, dt_star
    integer, intent(in) :: n_points
    integer :: i

    ! Initial normalized values
    z_star(1) = z0
    v_star(1) = v0

    ! Time array in normalized terms
    do i = 1, n_points
      time_star_array(i) = (i - 1) * dt_star
    end do
  end subroutine initialize_simulation_normalized

  ! Subroutine to compute normalized forces
  subroutine compute_forces_normalized(z_star, v_star, k_star, c_star, F_g_star, F_contact_star, F_net_star, include_damping)
    implicit none
    real, intent(in) :: z_star, v_star, k_star, c_star
    logical, intent(in), optional :: include_damping
    real, intent(out) :: F_g_star, F_contact_star, F_net_star

    ! Gravitational force (always -1 in normalized terms)
    F_g_star = -1.0

    ! Contact force
    if (z_star <= 0.0) then
      if (present(include_damping) .and. include_damping) then
        F_contact_star = -k_star * z_star - c_star * v_star
      else
        F_contact_star = -k_star * z_star
      end if
    else
      F_contact_star = 0.0
    end if

    ! Net force
    F_net_star = F_g_star + F_contact_star
  end subroutine compute_forces_normalized

  ! Subroutine to run the normalized simulation
  subroutine run_simulation_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, k_star, c_star, dt_star, epsilon, n_points, include_damping, delta_star_array, energy_star_array)
    implicit none
    real, intent(inout) :: z_star(:), v_star(:), time_star_array(:), energy_star_array(:)
    real, intent(out) :: F_g_star_array(:), F_contact_star_array(:), F_net_star_array(:), delta_star_array(:)
    real, intent(in) :: k_star, c_star, dt_star, epsilon
    logical, intent(in) :: include_damping
    integer, intent(in) :: n_points
    real :: F_g_star, F_contact_star, F_net_star, a_star, delta_star, initial_energy_star, current_energy_star
    integer :: i

    ! Initialize energy calculation
    initial_energy_star = 0.5 * v_star(1)**2 + z_star(1)
    energy_star_array(1) = initial_energy_star

    do i = 2, n_points
      ! Compute normalized forces
      call compute_forces_normalized(z_star(i - 1), v_star(i - 1), k_star, c_star, F_g_star, F_contact_star, F_net_star, include_damping)

      ! Store forces
      F_g_star_array(i) = F_g_star
      F_contact_star_array(i) = F_contact_star
      F_net_star_array(i) = F_net_star

      ! Compute normalized acceleration
      a_star = F_net_star

      ! Update normalized velocity and position
      v_star(i) = v_star(i - 1) + a_star * dt_star
      z_star(i) = z_star(i - 1) + v_star(i) * dt_star

      ! Compute penetration depth (delta)
      if (z_star(i) < 0.0) then
        delta_star = -z_star(i)
      else
        delta_star = 0.0
      end if
      delta_star_array(i) = delta_star

      ! Compute current normalized energy
      current_energy_star = 0.5 * v_star(i)**2 + z_star(i)
      energy_star_array(i) = current_energy_star

      ! Stop simulation if the velocity and height are small
      if (abs(v_star(i)) < epsilon .and. z_star(i) == 0.0) then
        z_star(i:) = 0.0
        v_star(i:) = 0.0
        F_g_star_array(i:) = 0.0
        F_contact_star_array(i:) = 0.0
        F_net_star_array(i:) = 0.0
        delta_star_array(i:) = 0.0
        exit
      end if
    end do
  end subroutine run_simulation_normalized

  ! Subroutine to write results for normalized simulation
  subroutine write_results_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, delta_star_array, energy_star_array, n_points, filename)
    implicit none
    real, intent(in) :: z_star(:), v_star(:), time_star_array(:), F_g_star_array(:), F_contact_star_array(:), F_net_star_array(:), delta_star_array(:), energy_star_array(:)
    integer, intent(in) :: n_points
    character(len=*), intent(in) :: filename
    integer :: i

    ! Open file to write results
    open(unit=10, file=filename, status="replace")
    write(10, '(A)') "Time*, Height*, Velocity*, F_g*, F_contact*, F_net*, Delta*, Energy*"

    ! Write normalized results
    do i = 1, n_points
      write(10, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') &
        time_star_array(i), z_star(i), v_star(i), F_g_star_array(i), F_contact_star_array(i), F_net_star_array(i), delta_star_array(i), energy_star_array(i)
    end do
    close(10)
  end subroutine write_results_normalized

  subroutine initialize_simulation_2D_normalized(x_star, y_star, vx_star, vy_star, time_star_array, x0, y0, vx0, vy0, dt_star, n_points)
    implicit none
    real, intent(out) :: x_star(:), y_star(:), vx_star(:), vy_star(:), time_star_array(:)
    real, intent(in) :: x0, y0, vx0, vy0, dt_star
    integer, intent(in) :: n_points
    integer :: i

    ! Initialize normalized arrays
    x_star(1) = x0
    y_star(1) = y0
    vx_star(1) = vx0
    vy_star(1) = vy0
    do i = 1, n_points
        time_star_array(i) = (i - 1) * dt_star
    end do
end subroutine initialize_simulation_2D_normalized

subroutine compute_forces_2D_normalized(x_star, y_star, vx_star, vy_star, k_star, c_star, mu_star, Fx_star, Fy_star, include_damping)
  implicit none
  real, intent(in) :: x_star, y_star, vx_star, vy_star, k_star, c_star, mu_star
  logical, intent(in), optional :: include_damping
  real, intent(out) :: Fx_star, Fy_star
  real :: delta_star, Fn_star, Ft_star

  ! Compute forces in normalized terms
  if (y_star <= 0.0) then
      delta_star = -y_star
      Fn_star = k_star * delta_star
      if (present(include_damping) .and. include_damping) then
          Fn_star = Fn_star - c_star * vy_star
      end if
      Ft_star = mu_star * Fn_star
      Fx_star = -Ft_star * sign(1.0, vx_star)
      Fy_star = Fn_star
  else
      Fx_star = 0.0
      Fy_star = -1.0  ! Gravitational force in normalized units
  end if
end subroutine compute_forces_2D_normalized

subroutine run_simulation_2D_normalized(x_star, y_star, vx_star, vy_star, time_star_array, Fx_star_array, Fy_star_array, k_star, c_star, mu_star, dt_star, epsilon, n_points, include_damping, delta_2D_star_array, energy_star_array)
  implicit none
  real, intent(inout) :: x_star(:), y_star(:), vx_star(:), vy_star(:), time_star_array(:), energy_star_array(:)
  real, intent(out) :: Fx_star_array(:), Fy_star_array(:), delta_2D_star_array(:)
  real, intent(in) :: k_star, c_star, mu_star, dt_star, epsilon
  logical, intent(in) :: include_damping
  integer, intent(in) :: n_points
  real :: Fx_star, Fy_star, ax_star, ay_star, delta_2D_star, initial_energy_star, current_energy_star
  integer :: i

  ! Initialize energy calculation
  initial_energy_star = 0.5 * (vx_star(1)**2 + vy_star(1)**2) + y_star(1)
  energy_star_array(1) = initial_energy_star

  do i = 2, n_points
      ! Compute forces
      call compute_forces_2D_normalized(x_star(i-1), y_star(i-1), vx_star(i-1), vy_star(i-1), k_star, c_star, mu_star, Fx_star, Fy_star, include_damping)

      ! Store forces
      Fx_star_array(i) = Fx_star
      Fy_star_array(i) = Fy_star

      ! Compute accelerations
      ax_star = Fx_star
      ay_star = Fy_star

      ! Update velocities
      vx_star(i) = vx_star(i-1) + ax_star * dt_star
      vy_star(i) = vy_star(i-1) + ay_star * dt_star

      ! Update positions
      x_star(i) = x_star(i-1) + vx_star(i) * dt_star
      y_star(i) = y_star(i-1) + vy_star(i) * dt_star

      ! Compute penetration depth
      if (y_star(i) < 0.0) then
          delta_2D_star = -y_star(i)
      else
          delta_2D_star = 0.0
      end if
      delta_2D_star_array(i) = delta_2D_star

      ! Compute current energy
      current_energy_star = 0.5 * (vx_star(i)**2 + vy_star(i)**2) + y_star(i)
      energy_star_array(i) = current_energy_star

      ! Stop simulation if velocity and height are small
      if (abs(vy_star(i)) < epsilon .and. y_star(i) == 0.0) then
          x_star(i:) = x_star(i-1)
          y_star(i:) = 0.0
          vx_star(i:) = 0.0
          vy_star(i:) = 0.0
          Fx_star_array(i:) = 0.0
          Fy_star_array(i:) = 0.0
          delta_2D_star_array(i:) = 0.0
          exit
      end if
  end do
end subroutine run_simulation_2D_normalized

subroutine write_results_2D(x_star, y_star, vx_star, vy_star, Fx_star_array, Fy_star_array, delta_2D_star_array, time_star_array, n_points, filename, energy_2D_star_array)
  implicit none
  real, intent(in) :: x_star(:), y_star(:), vx_star(:), vy_star(:), Fx_star_array(:), Fy_star_array(:), delta_2D_star_array(:), time_star_array(:), energy_2D_star_array(:)
  integer, intent(in) :: n_points
  character(len=*), intent(in) :: filename
  integer :: i

  ! Open the file for writing
  open(unit=10, file=filename, status="replace")
  write(10, '(A)') "Time*, X*, Y*, Vx*, Vy*, Fx*, Fy*, Delta*, Energy*"

  ! Write data
  do i = 1, n_points
      write(10, '(F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3, F10.3)') &
          time_star_array(i), x_star(i), y_star(i), vx_star(i), vy_star(i), Fx_star_array(i), Fy_star_array(i), delta_2D_star_array(i), energy_2D_star_array(i)
  end do

  close(10)
end subroutine write_results_2D



  subroutine generate_plot()
    implicit none

    open(unit=20, file="plot_commands_normalized.gp", status="replace")

    ! Height vs. Time for Elastic Case
    write(20, *) "set title 'Normalized Height vs. Time (Elastic Case)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Height*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'height_vs_time_elastic.png'"
    write(20, *) "plot 'results_normalized_1D.txt' using 1:2 with lines title 'Height* (z*)'"

    ! Delta vs. Time for Elastic Case
    write(20, *) "set title 'Normalized Delta vs. Time (Elastic Case)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Delta*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'delta_vs_time_elastic.png'"
    write(20, *) "plot 'results_normalized_1D.txt' using 1:7 with lines title 'Delta* (Elastic)'"

    ! Energy vs. Time for Elastic Case
    write(20, *) "set title 'Normalized Energy vs. Time (Elastic Case)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Energy*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'energy_vs_time_elastic.png'"
    write(20, *) "plot 'results_normalized_1D.txt' using 1:8 with lines title 'Energy* (Elastic)'"

    ! Net Force vs. Time for Elastic Case
    write(20, *) "set title 'Normalized Net Force vs. Time (Elastic Case)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Net Force*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'net_force_vs_time_elastic.png'"
    write(20, *) "plot 'results_normalized_1D.txt' using 1:6 with lines title 'Net Force* (Elastic)'"

    ! Delta vs. Net Force for Elastic Case
    write(20, *) "set title 'Normalized Delta vs. Net Force (Elastic Case)'"
    write(20, *) "set xlabel 'Delta*'"
    write(20, *) "set ylabel 'Net Force*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'delta_vs_net_force_elastic.png'"
    write(20, *) "plot 'results_normalized_1D.txt' using 7:6 with lines title 'Delta* vs. Net Force*'"

        ! 2D Trajectory (Y* vs X*)
    write(20, *) "set title 'Normalized 2D Trajectory (Y* vs X*)'"
    write(20, *) "set xlabel 'X*'"
    write(20, *) "set ylabel 'Y*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'trajectory_2D_normalized.png'"
    write(20, *) "plot 'results_normalized_2D.txt' using 2:3 with lines title 'Trajectory (Y* vs X*)'"

    ! Velocity Components vs Time*
    write(20, *) "set title 'Normalized Velocity Components vs Time* (2D)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Velocity*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'velocity_components_2D_normalized.png'"
    write(20, *) "plot 'results_normalized_2D.txt' using 1:4 with lines title 'Vx*', '' using 1:5 with lines title 'Vy*'"

    ! Forces vs Time*
    write(20, *) "set title 'Normalized Forces vs Time* (2D)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Force*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'forces_vs_time_2D_normalized.png'"
    write(20, *) "plot 'results_normalized_2D.txt' using 1:6 with lines title 'Fx*', '' using 1:7 with lines title 'Fy*'"

    ! Energy vs Time*
    write(20, *) "set title 'Normalized Energy vs Time* (2D)'"
    write(20, *) "set xlabel 'Time*'"
    write(20, *) "set ylabel 'Energy*'"
    write(20, *) "set grid"
    write(20, *) "set terminal png size 800,600"
    write(20, *) "set output 'energy_vs_time_2D_normalized.png'"
    write(20, *) "plot 'results_normalized_2D.txt' using 1:9 with lines title 'Energy*'"


    close(20)

    ! Execute the Gnuplot script
    call execute_command_line("gnuplot plot_commands_normalized.gp")
end subroutine generate_plot

end module simulation_module

! program particle_motion_normalized
!   use simulation_module
!   implicit none

!   ! Variables for normalized simulation
!   real :: dt_star, T_star, z0_star, v0_star, k_star, c_star, epsilon
!   real, allocatable :: z_star(:), v_star(:), time_star_array(:), F_g_star_array(:), F_contact_star_array(:), F_net_star_array(:), delta_star_array(:), energy_star_array(:)
!   integer :: n_points
!   logical :: include_damping

!   ! Normalized parameters
!   z0_star = 1.0
!   v0_star = 0.0
!   k_star = 100.0  ! Example normalized spring constant
!   c_star = 0.05    ! Example normalized damping coefficient
!   dt_star = 0.00001  ! Normalized time step
!   T_star = 100.0     ! Normalized total simulation time
!   epsilon = 1e-3   ! Normalized stopping threshold

!   ! Number of points
!   n_points = int(T_star / dt_star) + 1

!   ! Allocate arrays
!   allocate(z_star(n_points), v_star(n_points), time_star_array(n_points))
!   allocate(F_g_star_array(n_points), F_contact_star_array(n_points), F_net_star_array(n_points), delta_star_array(n_points), energy_star_array(n_points))

!   ! Initialize simulation
!   include_damping = .false.
!   call initialize_simulation_normalized(z_star, v_star, time_star_array, z0_star, v0_star, dt_star, n_points)

!   ! Run simulation
!   call run_simulation_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, k_star, c_star, dt_star, epsilon, n_points, include_damping, delta_star_array, energy_star_array)

!   ! Write results
!   call write_results_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, delta_star_array, energy_star_array, n_points, "results_normalized.txt")

!   ! Generate plots for normalized simulation
!   call generate_plot()

!   ! Deallocate arrays
!   deallocate(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, delta_star_array, energy_star_array)
! end program particle_motion_normalized

