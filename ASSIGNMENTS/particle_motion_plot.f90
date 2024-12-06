program particle_motion
  use simulation_module, only: initialize_simulation, initialize_simulation_2D, compute_forces, compute_forces_2D, run_simulation, run_simulation_2D, write_results, write_results_2D, generate_plot
  implicit none

  ! Variables for 1D simulation and 2D simulation
  real :: dt, T, z0, v0, k, c, m, g, epsilon, mu, R
  real, allocatable :: z(:), v(:), time_array(:), delta_array(:)
  real, allocatable :: x(:), y(:), vx(:), vy(:), Fx_array(:), Fy_array(:), F_g_array(:), F_contact_array(:), F_net_array(:)
  integer :: n_points
  logical :: include_damping

  ! Initialize parameters
  m = 1.0        ! Mass (kg)
  g = 9.81       ! Gravitational acceleration (m/s^2)
  z0 = 1.0      ! Initial height (m)
  v0 = 0.0       ! Initial velocity (m/s)
  k = 1000.0     ! Spring constant (N/m)
  c = 10.0       ! Damping coefficient (Ns/m)
  dt = 0.001     ! Time step (s)
  T = 5.0        ! Total simulation time (s)
  epsilon = 1e-3 ! Stopping threshold
  mu = 0.5       ! Friction coefficient
  R = 1.0        ! Radius of the Particle (m)

  ! Compute the number of points
  n_points = int(T / dt) + 1

  ! Allocate arrays
  allocate(z(n_points), v(n_points), time_array(n_points))
  allocate(F_g_array(n_points), F_contact_array(n_points), F_net_array(n_points), delta_array(n_points))

  ! Allocate arrays for 2D simulation
  allocate(x(n_points), y(n_points), vx(n_points), vy(n_points), Fx_array(n_points), Fy_array(n_points))


  ! ! Initialize arrays
  ! call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)

  ! ! Perform the simulation
  ! call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points)

  ! ! Output results to a file
  ! call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, n_points)

  ! ! Generate plot using Gnuplot
  ! call generate_plot()

  ! --- Case 1: 1D Elastic Only ---
  include_damping = .false.  ! No damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_elastic.txt")
  call generate_plot()  ! Save plot for elastic case

  ! --- Case 2: 1D Elastic + Damping ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping.txt")
  call generate_plot()  ! Save plot for elastic + damping case

  ! --- Case 2: 1D Elastic + Damping (Net_Force) ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R) 
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping_F_net.txt")
  call generate_plot()  ! Save plot for elastic + damping case

  ! --- Case 2: 1D Elastic + Damping (Net_Force_vs_Time) ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping_F_net_vs_Time.txt")
  call generate_plot()  ! Save plot for elastic + damping case

  ! --- Case 2: 1D Elastic + Damping (Contact_Force) ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping_F_contact.txt")
  call generate_plot()  ! Save plot for elastic + damping case

  ! --- Case 2: 1D Elastic + Damping (Contact_Force_vs_Time) ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping_F_contact_vs_Time.txt")
  call generate_plot()  ! Save plot for elastic + damping case

  ! --- Case 2: 1D Elastic + Damping (Gravitational_Force) ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping, delta_array, R)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_array, n_points, "results_damping_F_g.txt")
  call generate_plot()  ! Save plot for elastic + damping case


  ! --- Case 3: 2D Elastic + Friction ---
  include_damping = .true.
  call initialize_simulation_2D(x, y, vx, vy, time_array, 0.0, 1.0, 1.0, 0.0, dt, n_points)
  call run_simulation_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, m, g, k, c, mu, dt, epsilon, n_points, include_damping, delta_array)
  call write_results_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, delta_array, n_points, "results_2D.txt")
  call generate_plot()  ! Save plot for 2D case

  
  ! Deallocate arrays
  deallocate(z, v, time_array, F_g_array, F_contact_array, F_net_array, x, y, vx, vy, Fx_array, Fy_array, delta_array)
end program particle_motion

