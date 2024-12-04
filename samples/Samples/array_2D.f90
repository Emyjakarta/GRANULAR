PROGRAM array_2D
    
    implicit none
    integer :: i, j, n_rows, n_cols
    ! Declare a 2D array with static allocation (3 rows and 4 columns)  
    real :: matrix(3, 4)
    ! Declare a dynamically allocated matrix (size will be defined during runtime)
    real, allocatable :: matrix2(:,:)

    !********************************
    ! 2D arrays - Static allocation
    !*******************************

    ! Initialize the matrix with values
    DO i = 1, 3
        DO j = 1, 4
            matrix(i, j) = REAL(i * j)  ! Assign values based on row and column indices
        END DO
    END DO

    ! Print the matrix elements
    PRINT *, "Static 2D array values:"
    DO i = 1, 3
        DO j = 1, 4
            PRINT *, "matrix(", i, ",", j, ") = ", matrix(i, j)
        END DO
    END DO

    !********************************
    ! 2D arrays - dynamic allocation
    !*******************************

    ! Ask the user for the size of the matrix (number of rows and columns)
    PRINT *, "Enter the number of rows:"
    READ *, n_rows
    PRINT *, "Enter the number of columns:"
    READ *, n_cols

    ! Allocate the vector based on the input size
    ALLOCATE(matrix2(n_rows, n_cols))

    ! Initialize the matrix with values
    DO i = 1, n_rows
        DO j = 1, n_cols
            matrix2(i, j) = REAL(i * j)  ! Assign values based on row and column indices
        END DO
    END DO

    ! Print the matrix elements
    PRINT *, "Dynamic 2D array values:"
    DO i = 1, n_rows
        DO j = 1, n_cols
            PRINT *, "matrix2(", i, ",", j, ") = ", matrix2(i, j)
        END DO
    END DO

    ! Deallocate the vector after use to free memory
    DEALLOCATE(matrix2)

END PROGRAM array_2D