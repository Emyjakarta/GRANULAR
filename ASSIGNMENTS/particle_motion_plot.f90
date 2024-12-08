program particle_motion ! Main program to simulate the motion of a particle in 1D and 2D
  use simulation_module, only: initialize_simulation, initialize_simulation_2D, compute_forces, compute_forces_2D, run_simulation, run_simulation_2D, write_results, write_results_2D, generate_plot ! Import subroutines from simulation_module
  implicit none ! Explicit variable declaration

  ! Variables for 1D simulation and 2D simulation
  real :: dt, T, z0, v0, k, c, m, g, epsilon, mu, R ! dt (time step), T (total simulation time), z0 (initial height), v0 (initial velocity), k (spring constant), c (damping coefficient), m (mass), g (gravitational acceleration), epsilon (stopping threshold), mu (friction coefficient), R (radius of the particle)
  real, allocatable :: z(:), v(:), time_array(:), delta_elastic_array(:), delta_damped_array(:), delta_2D_array(:), energy_array_1D(:), energy_array_2D(:)
  real, allocatable :: x(:), y(:), vx(:), vy(:), Fx_array(:), Fy_array(:), F_g_array(:), F_contact_array(:), F_net_array(:)
  integer :: n_points
  logical :: include_damping ! Flag to include damping in the simulation

  ! Initialize parameters
  m = 1.0        ! Mass (kg) 
  g = 9.81       ! Gravitational acceleration (m/s^2)
  z0 = 10.0      ! Initial height (m)
  v0 = 0.0       ! Initial velocity (m/s)
  k = 1000 ! 70000000.0 ! Spring constant (N/m)
  c = 0.05       ! Damping coefficient for damped case(Ns/m)
  dt = 0.0001  ! Time step (s)
  T = 5.0        ! Total simulation time (s)
  epsilon = 1e-3 ! Stopping threshold
  mu = 0.5       ! Friction coefficient
  R = 0.12       ! Radius of the Particle (m)

  ! Compute the number of points
  n_points = int(T / dt) + 1 ! Number of points in the simulation

  ! Allocate arrays for 1D simulation
  allocate(z(n_points), v(n_points), time_array(n_points)) ! Allocate arrays for position, velocity, and time
  allocate(F_g_array(n_points), F_contact_array(n_points), F_net_array(n_points), delta_elastic_array(n_points), delta_damped_array(n_points), delta_2D_array(n_points)) ! Allocate arrays for forces  
  allocate(energy_array_1D(n_points), energy_array_2D(n_points)) ! Allocate arrays for energy

  ! Allocate arrays for 2D simulation 
  allocate(x(n_points), y(n_points), vx(n_points), vy(n_points), Fx_array(n_points), Fy_array(n_points)) ! Allocate arrays for position, velocity, and forces

  ! --- Case 1: 1D Elastic Only --- 
  include_damping = .false.  ! No damping 
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points) ! Initialize simulation
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, .false., delta_elastic_array, delta_damped_array, R, energy_array_1D)  
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_elastic_array, n_points, "results_elastic.txt", energy_array_1D)  ! Write results to file 
  call generate_plot()  ! Save plot for elastic case

  ! --- Case 2: 1D Damping ---
  include_damping = .true.  ! Include damping
  call initialize_simulation(z, v, time_array, z0, v0, dt, n_points)
  call run_simulation(z, v, time_array, F_g_array, F_contact_array, F_net_array, m, g, k, c, dt, epsilon, n_points, .true., delta_elastic_array, delta_damped_array, R, energy_array_1D) 
  call write_results(z, v, time_array, F_g_array, F_contact_array, F_net_array, delta_damped_array, n_points, "results_damping.txt", energy_array_1D)  ! Write results to file 
  call generate_plot()  ! Save plot for elastic + damping case


  ! --- Case 3: 2D Elastic + Friction ---
  include_damping = .true.
  call initialize_simulation_2D(x, y, vx, vy, time_array, 0.0, 1.0, 1.0, 0.0, dt, n_points)
  call run_simulation_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, m, g, k, c, mu, dt, epsilon, n_points, .true., delta_2D_array, energy_array_2D) 
  call write_results_2D(x, y, vx, vy, time_array, Fx_array, Fy_array, delta_2D_array, n_points, "results_2D.txt", energy_array_2D)  ! Write results to file  
  call generate_plot()  ! Save plot for 2D case

  
  ! Deallocate arrays beacuse we are done with the simulation
  deallocate(z, v, time_array, F_g_array, F_contact_array, F_net_array, x, y, vx, vy, Fx_array, Fy_array, energy_array_1D, energy_array_2D)  
end program particle_motion ! End of the program