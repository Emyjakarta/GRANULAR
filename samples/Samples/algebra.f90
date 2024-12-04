PROGRAM AlgebraQuadraticSolution
    IMPLICIT NONE
    REAL :: a, b, c, D, x1, x2

    ! Make the program interactive. Ask the user to input the coefficients of the quadratic equation
    PRINT *, "Enter coefficients a, b, and c for ax^2 + bx + c = 0:"
    READ *, a, b, c

    ! Calculate D
    D = b**2 - 4.0 * a * c

    ! Define the conditions for checking D and solve
    IF (D > 0.0) THEN
        x1 = (-b + SQRT(D)) / (2.0 * a)
        x2 = (-b - SQRT(D)) / (2.0 * a)
        PRINT *, "The coefficients produced real and distinct roots:"
        PRINT *, "x1 =", x1
        PRINT *, "x2 =", x2
    ELSE IF (D == 0.0) THEN
        x1 = -b / (2.0 * a)
        PRINT *, "The coefficients produced real and equal roots:"
        PRINT *, "x1 = x2 =", x1
    ELSE
        PRINT *, "The coefficients produced complex roots because the discriminant is negative."
    END IF
END PROGRAM AlgebraQuadraticSolution

