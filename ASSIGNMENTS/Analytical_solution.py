import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Constants from the problem
k = 100  # Spring constant (N/m)
m1 = 5.0  # Mass of particle 1 (kg)
m2 = 5.0  # Mass of particle 2 (kg)
delta_n_0 = 0.01  # Initial displacement (m)
time_step = 0.0001  # Time step for simulation (s)
total_time = 0.05  # Total simulation time (s)

# Derived quantities
m_star = 1 / ((1 / m1) + (1 / m2))  # Reduced mass (kg)
omega = np.sqrt(k / m_star)  # Angular frequency (rad/s)

# Time array
time_array = np.arange(0, total_time, time_step)

# Analytical solution for delta_n(t)
delta_n_t = delta_n_0 * np.sin(omega * time_array)

# Plotting the analytical solution
plt.figure(figsize=(10, 6))
plt.plot(time_array, delta_n_t, label=r"Analytical $\delta_n(t)$", color="blue")
plt.title(r"Analytical Solution for $\delta_n(t)$ vs. Time")
plt.xlabel("Time (s)")
plt.ylabel(r"Displacement $\delta_n(t)$ (m)")
plt.grid(True)
plt.legend()
plt.show()

# Save data for comparison
analytical_results = np.column_stack((time_array, delta_n_t))
output_file = "analytical_delta_time.csv"  # Save in current directory
pd.DataFrame(analytical_results, columns=["Time (s)", "Delta_n (m)"]).to_csv(output_file, index=False)
print(f"Data saved to {output_file}")

