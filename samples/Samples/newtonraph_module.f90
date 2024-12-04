MODULE NewtonRaphModule
    IMPLICIT NONE
CONTAINS
    ! Function to compute the value of f(x)
    FUNCTION ComputeMainFunction(a, b, c, x) RESULT(f)
        REAL, INTENT(IN) :: a, b, c, x
        REAL :: f
        f = a * x**2 + b * x + c
    END FUNCTION ComputeMainFunction

    ! Function to compute the value of f'(x)
    FUNCTION ComputeDer(a, b, x) RESULT(df)
        REAL, INTENT(IN) :: a, b, x
        REAL :: df
        df = 2.0 * a * x + b
    END FUNCTION ComputeDer

    ! Subroutine for the Newton-Raphson method
    SUBROUTINE NewtonRaphSolver(a, b, c, x, tol, max_iter)
        IMPLICIT NONE
        REAL, INTENT(IN) :: a, b, c, tol
        INTEGER, INTENT(IN) :: max_iter
        REAL, INTENT(INOUT) :: x
        REAL :: f, df
        INTEGER :: i

        DO i = 1, max_iter
            f = ComputeMainFunction(a, b, c, x)
            df = ComputeDer(a, b, x)

            ! Avoid division by zero
            IF (df == 0.0) THEN
                PRINT *, "Derivative is zero. Stopping iterations."
                EXIT
            END IF

            x = x - f / df ! Update x using Newton-Raphson formula

            ! Check for convergence
            IF (ABS(f) < tol) THEN
                PRINT *, "Converged to root:", x
                ! EXIT
                RETURN
            END IF
        END DO

        IF (i > max_iter) THEN
            PRINT *, "Did not converge within the maximum iterations."
        END IF
    END SUBROUTINE NewtonRaphSolver
END MODULE NewtonRaphModule

