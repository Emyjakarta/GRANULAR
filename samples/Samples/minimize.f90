PROGRAM MinimizeDistance
    IMPLICIT NONE
    REAL :: theta, phi, theta_new, phi_new, dF_dtheta, dF_dphi
    REAL :: xC, yC, R, a, b, alpha, tol
    REAL :: F_min, d_min 
    INTEGER :: max_iter, i

    ! Input parameters
    PRINT *, "Enter ellipse semi-axes (a, b):"
    READ *, a, b
    PRINT *, "Enter sphere center (xC, yC) and radius R:"
    READ *, xC, yC, R
    PRINT *, "Enter initial guess for (theta, phi):"
    READ *, theta, phi
    PRINT *, "Enter learning rate (alpha) and tolerance:"
    READ *, alpha, tol
    PRINT *, "Enter maximum iterations:"
    READ *, max_iter

    ! Gradient Descent iterations
    DO i = 1, max_iter
        ! Compute gradients
        dF_dtheta = -2.0 * (xC + R * COS(phi) - a * COS(theta)) * a * SIN(theta) &
                    -2.0 * (yC + R * SIN(phi) - b * SIN(theta)) * b * COS(theta)

        dF_dphi = -2.0 * (xC + R * COS(phi) - a * COS(theta)) * (-R * SIN(phi)) &
                  -2.0 * (yC + R * SIN(phi) - b * SIN(theta)) * (R * COS(phi))

        ! Update variables
        theta_new = theta - alpha * dF_dtheta
        phi_new = phi - alpha * dF_dphi

        ! Check for convergence
        IF (ABS(theta_new - theta) < tol .AND. ABS(phi_new - phi) < tol) THEN
            PRINT *, "Converged to (theta, phi):", theta_new, phi_new

            ! Compute minimal squared distance and minimal distance

            F_min = (xC + R * COS(phi_new) - a * COS(theta_new))**2 + &
                    (yC + R * SIN(phi_new) - b * SIN(theta_new))**2
            d_min = SQRT(F_min)

            PRINT *, "Minimal squared distance F(theta, phi):", F_min
            PRINT *, "Minimal distance d(theta, phi):", d_min

            EXIT
        END IF

        theta = theta_new
        phi = phi_new
    END DO

    IF (i > max_iter) THEN
        PRINT *, "Did not converge within the maximum iterations."
    END IF
END PROGRAM MinimizeDistance

