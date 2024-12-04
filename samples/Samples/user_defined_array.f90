PROGRAM user_defined_array
    
    implicit none
    
    INTEGER :: i, num_particles

    ! Define a user-defined type for Particle
    TYPE :: Particle
        REAL :: position(3)   ! 3D position (x, y, z)
        REAL :: velocity(3)   ! 3D velocity (vx, vy, vz)
    END TYPE Particle

    ! Declare a static array of particles with a fixed size
    TYPE(Particle) :: particles(5)   ! We know there are 5 particles

    ! Declare an allocatable array of particles (dynamic allocation)
    TYPE(Particle), ALLOCATABLE :: particles2(:)

    !*************************************************
    ! Array of User-defined Type - Static allocation
    !*************************************************

    ! Initialize the particles with some example values
    DO i = 1, 5
        particles(i)%position = (/ REAL(i), REAL(i+1), REAL(i+2) /)  ! Initialize position
        particles(i)%velocity = (/ REAL(i*0.1), REAL(i*0.2), REAL(i*0.3) /)  ! Initialize velocity
    END DO

    ! Print the position and velocity of each particle
    PRINT *, "Particle positions and velocities:"
    DO i = 1, 5
        PRINT *, "Particle ", i
        PRINT *, "  Position: ", particles(i)%position
        PRINT *, "  Velocity: ", particles(i)%velocity
    END DO

    !*************************************************
    ! Array of User-defined Type - Dynamic allocation
    !*************************************************

    ! Ask the user for the number of particles
    PRINT *, "Enter the number of particles:"
    READ *, num_particles

    ! Allocate the array of particles based on user input
    ALLOCATE(particles2(num_particles))

    ! Initialize the particles with some example values
    DO i = 1, num_particles
        particles2(i)%position = (/ REAL(i), REAL(i+1), REAL(i+2) /)  ! Initialize position
        particles2(i)%velocity = (/ REAL(i*0.1), REAL(i*0.2), REAL(i*0.3) /)  ! Initialize velocity
    END DO

    ! Print the position and velocity of each particle
    PRINT *, "Particle positions and velocities:"
    DO i = 1, num_particles
        PRINT *, "Particle ", i
        PRINT *, "  Position: ", particles2(i)%position
        PRINT *, "  Velocity: ", particles2(i)%velocity
    END DO

    ! Deallocate the particle array after use
    DEALLOCATE(particles2)

END PROGRAM user_defined_array