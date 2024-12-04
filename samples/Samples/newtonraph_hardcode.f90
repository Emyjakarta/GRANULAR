MODULE NewtonRaphModule
    IMPLICIT NONE
CONTAINS
    ! Function to compute the value of f(x)
    FUNCTION ComputeMainFunction(a, b, c, x) RESULT(f)
        DOUBLE PRECISION, INTENT(IN) :: a, b, c, x
        DOUBLE PRECISION :: f
        f = a * x**2 + b * x + c
    END FUNCTION ComputeMainFunction

    ! Function to compute the value of f'(x)
    FUNCTION ComputeDer(a, b, x) RESULT(df)
        DOUBLE PRECISION, INTENT(IN) :: a, b, x
        DOUBLE PRECISION :: df
        df = 2.0D0 * a * x + b
    END FUNCTION ComputeDer

    ! Subroutine for the Newton-Raphson method
    SUBROUTINE NewtonRaphSolver(a, b, c, x, tol, max_iter)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: a, b, c, tol
        INTEGER, INTENT(IN) :: max_iter
        DOUBLE PRECISION, INTENT(INOUT) :: x
        DOUBLE PRECISION :: f, df
        INTEGER :: i

        DO i = 1, max_iter
            f = ComputeMainFunction(a, b, c, x)
            df = ComputeDer(a, b, x)

            ! Avoid division by zero
            IF (df == 0.0D0) THEN
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


