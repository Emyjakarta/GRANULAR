program minimize_distance
    use parameters
    use functions
    implicit none

    ! Local variables
    real :: theta, phi, F, F_prev
    real :: gradient_theta, gradient_phi
    integer :: iter

    ! Initialization
    call initialize_parameters()
    theta = 0.0
    phi = 0.0
    iter = 0

    ! Optimization loop
    do
        ! Compute objective function
        F = objective_function(a, b, R, xC, yC, theta, phi)

        ! Compute gradients
        gradient_theta = gradient_theta_func(a, b, R, xC, yC, theta, phi)
        gradient_phi = gradient_phi_func(a, b, R, xC, yC, theta, phi)

        ! Update theta and phi
        theta = theta - alpha * gradient_theta
        phi = phi - alpha * gradient_phi

        ! Ensure theta and phi remain within [0, 2*pi]
        theta = mod(theta, 2.0 * pi)
        phi = mod(phi, 2.0 * pi)

        ! Check convergence
        if (iter > 0 .and. abs(F - F_prev) < tolerance) then
            print *, "Converged after ", iter, " iterations"
            exit
        endif

        ! Update previous value of F
        F_prev = F
        iter = iter + 1

        ! Stop if max iterations reached
        if (iter >= max_iter) then
            print *, "Max iterations reached"
            exit
        endif
    end do

    ! Output results
    print *, "Minimum squared distance: ", F
    print *, "Optimal theta: ", theta
    print *, "Optimal phi: ", phi
contains
    ! Initialization subroutine
    subroutine initialize_parameters()
        use parameters
        implicit none
        a = 3.0            ! Semi-major axis of the ellipse
        b = 2.0            ! Semi-minor axis of the ellipse
        R = 4.0            ! Radius of the sphere
        xC = 5.0           ! x-coordinate of the sphere's center
        yC = 6.0           ! y-coordinate of the sphere's center
        alpha = 0.01       ! Learning rate
        tolerance = 1.0E-6 ! Convergence tolerance
        max_iter = 10000   ! Maximum number of iterations
    end subroutine initialize_parameters
end program minimize_distance

