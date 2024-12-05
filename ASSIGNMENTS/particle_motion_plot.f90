program particle_motion
  use simulation_module, only: initialize_simulation, compute_forces, run_simulation, write_results, generate_plot
  implicit none

  ! Variables
  real :: dt, T, z0, v0, k, c, m, g, epsilon
  real, allocatable :: z(:), v(:), time_array(:)
  real, allocatable :: F_g_array(:), F_contact_array(:), F_net_array(:)
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

  ! Compute the number of points
  n_points = int(T / dt) + 1

  ! Allocate arrays
  allocate(z(n_points), v(n_points), time_array(n_points))
  allocate(F_g_array(n_points), F_contact_array(n_points), F_net_array(n_points))


  ! ! Initialize arrays
  ! call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)

  ! ! Perform the simulation
  ! call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points)

  ! ! Output results to a file
  ! call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, n_points)

  ! ! Generate plot using Gnuplot
  ! call generate_plot()

  ! --- Case 1: Elastic Only ---
  include_damping = .false.  ! No damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, n_points, "results_elastic.txt")
  call generate_plot()  ! Save plot for elastic case

  ! --- Case 2: Elastic + Damping ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, include_damping)
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, n_points, "results_damping.txt")
  call generate_plot()  ! Save plot for elastic + damping case


  ! Deallocate arrays
  deallocate(z, v, time_array, F_g_array, F_contact_array, F_net_array)
end program particle_motion

