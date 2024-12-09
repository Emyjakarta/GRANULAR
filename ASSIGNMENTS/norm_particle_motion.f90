program particle_motion_normalized
  use simulation_module
  implicit none

  ! Variables for normalized simulation
  real :: dt_star, T_star, z0_star, v0_star, x0_star, y0_star, vx0_star, vy0_star, k_star, c_star, mu_star, epsilon
  real, allocatable :: z_star(:), v_star(:), time_star_array(:), F_g_star_array(:), F_contact_star_array(:), F_net_star_array(:), delta_star_array(:), energy_star_array(:)
  real, allocatable :: x_star(:), y_star(:), vx_star(:), vy_star(:), Fx_star_array(:), Fy_star_array(:), delta_2D_star_array(:), energy_2D_star_array(:)
  integer :: n_points
  logical :: include_damping

  ! Normalized parameters
  z0_star = 1.0
  v0_star = 0.0
  x0_star = 0.0
  y0_star = 1.0
  vx0_star = 0.1
  vy0_star = 0.0
  k_star = 100.0       ! Normalized spring constant
  c_star = 0.05        ! Normalized damping coefficient
  mu_star = 0.3        ! Normalized friction coefficient
  dt_star = 0.001    ! Normalized time step
  T_star = 100.0       ! Normalized total simulation time
  epsilon = 1e-3       ! Normalized stopping threshold

  ! Number of points
  n_points = int(T_star / dt_star) + 1

  ! Allocate arrays for 1D simulation
  allocate(z_star(n_points), v_star(n_points), time_star_array(n_points))
  allocate(F_g_star_array(n_points), F_contact_star_array(n_points), F_net_star_array(n_points), delta_star_array(n_points), energy_star_array(n_points))

  ! Allocate arrays for 2D simulation
  allocate(x_star(n_points), y_star(n_points), vx_star(n_points), vy_star(n_points))
  allocate(Fx_star_array(n_points), Fy_star_array(n_points), delta_2D_star_array(n_points), energy_2D_star_array(n_points))

  ! --- 1D Simulation ---
  include_damping = .false.
  call initialize_simulation_normalized(z_star, v_star, time_star_array, z0_star, v0_star, dt_star, n_points)
  call run_simulation_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, k_star, c_star, dt_star, epsilon, n_points, include_damping, delta_star_array, energy_star_array)
  call write_results_normalized(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, delta_star_array, energy_star_array, n_points, "results_normalized_1D.txt")

  ! --- 2D Simulation ---
  include_damping = .true.
  call initialize_simulation_2D_normalized(x_star, y_star, vx_star, vy_star, time_star_array, x0_star, y0_star, vx0_star, vy0_star, dt_star, n_points)
  call run_simulation_2D_normalized(x_star, y_star, vx_star, vy_star, time_star_array, Fx_star_array, Fy_star_array, k_star, c_star, mu_star, dt_star, epsilon, n_points, include_damping, delta_2D_star_array, energy_2D_star_array)
  call write_results_2D(x_star, y_star, vx_star, vy_star, Fx_star_array, Fy_star_array, delta_2D_star_array, time_star_array, n_points, "results_normalized_2D.txt", energy_2D_star_array)

  ! Generate plots for normalized simulations
  call generate_plot()

  ! Deallocate arrays
  deallocate(z_star, v_star, time_star_array, F_g_star_array, F_contact_star_array, F_net_star_array, delta_star_array, energy_star_array)
  deallocate(x_star, y_star, vx_star, vy_star, Fx_star_array, Fy_star_array, delta_2D_star_array, energy_2D_star_array)
end program particle_motion_normalized

