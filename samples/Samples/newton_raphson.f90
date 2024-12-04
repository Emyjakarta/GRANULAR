PROGRAM NewtonRaphsonSolver
    IMPLICIT NONE
    REAL :: a, b, c, x, tol, f, df
    INTEGER :: max_iter, i

    ! Input coefficients and initial guess
    PRINT *, "Enter coefficients a, b, and c for ax^2 + bx + c = 0:"
    READ *, a, b, c
    PRINT *, "Enter initial guess and tolerance:"
    READ *, x, tol
    PRINT *, "Enter maximum iterations:"
    READ *, max_iter

    ! Newton-Raphson iterations
    DO i = 1, max_iter
        f = a * x**2 + b * x + c
        df = 2.0 * a * x + b

        ! Avoid division by zero
        IF (df == 0.0) THEN
            PRINT *, "Derivative is zero. Stopping iterations."
            EXIT
        END IF

        x = x - f / df

        ! Check for convergence
        IF (ABS(f) < tol) THEN
            PRINT *, "Converged to root:", x
            EXIT
        END IF
    END DO

    IF (i > max_iter) THEN
        PRINT *, "Did not converge within the maximum iterations."
    END IF
END PROGRAM NewtonRaphsonSolver

