PROGRAM array_1D
    
    implicit none
    integer :: i, n
    ! Declare a vector of size 5 with static allocation (once for all, at the beginning)
    real :: vector(5)
    ! Declare a dynamically allocated vector (size will be defined during runtime)
    real, allocatable :: vector2(:)


    !********************************
    ! 1D arrays - Static allocation
    !*******************************

    ! Initialize the vector with values
    DO i = 1, 5
        vector(i) = REAL(i)  ! Assign values 1.0, 2.0, 3.0, etc.
    END DO

    ! Print the vector elements
    PRINT *, "Static vector values:"
    DO i = 1, 5
        PRINT *, "vector(", i, ") = ", vector(i)
    END DO

    !********************************
    ! 1D arrays - dynamic allocation
    !*******************************

    ! Ask the user for the size of the vector (at runtime)
    PRINT *, "Enter the size of the vector:"
    READ *, n

    ! Allocate the vector based on the input size
    ALLOCATE(vector2(n))

    ! Initialize the vector with values
    DO i = 1, n
        vector2(i) = REAL(i)  ! Assign values 1.0, 2.0, 3.0, etc.
    END DO

    ! Print the vector elements
    PRINT *, "Dynamic vector values:"
    DO i = 1, n
        PRINT *, "vector(", i, ") = ", vector2(i)
    END DO

    ! Deallocate the vector after use to free memory
    DEALLOCATE(vector2)

END PROGRAM array_1D