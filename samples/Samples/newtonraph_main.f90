PROGRAM MainNewtonRaphson
    USE NewtonRaphModule
    IMPLICIT NONE
    REAL :: a, b, c, x1, x2, tol
    INTEGER :: max_iter

    ! Input coefficients and initial guess
    PRINT *, "Enter coefficients a, b, and c for ax^2 + bx + c = 0:"
    READ *, a, b, c
    PRINT *, "Enter first initial guess and tolerance:"
    READ *, x1, tol
    PRINT *, "Enter second initial guess:"
    READ *, x2
    PRINT *, "Enter maximum iterations:"
    READ *, max_iter

    ! Solve for the first root
    PRINT *, "Solving for the first root..."
    CALL NewtonRaphSolver(a, b, c, x1, tol, max_iter)

    ! Solve for the second root
    PRINT *, "Solving for the second root..."
    CALL NewtonRaphSolver(a, b, c, x2, tol, max_iter)

    ! Call the Newton-Raphson solver
    ! CALL NewtonRaphSolver(a, b, c, x, tol, max_iter)
END PROGRAM MainNewtonRaphson

