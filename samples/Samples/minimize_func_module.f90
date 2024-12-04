module parameters
    implicit none
    real :: a, b, R, xC, yC       ! Ellipse and sphere parameters
    real :: alpha, tolerance      ! Learning rate and convergence tolerance
    integer :: max_iter           ! Maximum number of iterations
    real, parameter :: pi = 3.141592653589793
end module parameters


module functions
    implicit none
contains
    ! Objective function F(theta, phi)
    real function objective_function(a, b, R, xC, yC, theta, phi)
        implicit none
        real, intent(in) :: a, b, R, xC, yC, theta, phi
        real :: xE, yE, xS, yS

        ! Compute the points on the ellipse and sphere
        xE = a * cos(theta)
        yE = b * sin(theta)
        xS = xC + R * cos(phi)
        yS = yC + R * sin(phi)

        ! Compute the squared distance
        objective_function = (xS - xE)**2 + (yS - yE)**2
    end function objective_function

    ! Gradient of F with respect to theta
    real function gradient_theta_func(a, b, R, xC, yC, theta, phi)
        implicit none
        real, intent(in) :: a, b, R, xC, yC, theta, phi
        real :: xE, yE, xS, yS

        ! Compute the points on the ellipse and sphere
        xE = a * cos(theta)
        yE = b * sin(theta)
        xS = xC + R * cos(phi)
        yS = yC + R * sin(phi)

        ! Gradient with respect to theta
        gradient_theta_func = -2.0 * a * sin(theta) * (xS - xE) + 2.0 * b * cos(theta) * (yS - yE)
    end function gradient_theta_func

    ! Gradient of F with respect to phi
    real function gradient_phi_func(a, b, R, xC, yC, theta, phi)
        implicit none
        real, intent(in) :: a, b, R, xC, yC, theta, phi
        real :: xE, yE, xS, yS

        ! Compute the points on the ellipse and sphere
        xE = a * cos(theta)
        yE = b * sin(theta)
        xS = xC + R * cos(phi)
        yS = yC + R * sin(phi)

        ! Gradient with respect to phi
        gradient_phi_func = 2.0 * R * sin(phi) * (xS - xE) + 2.0 * R * cos(phi) * (yS - yE)
    end function gradient_phi_func
end module functions

