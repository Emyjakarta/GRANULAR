PROGRAM type_conversion

    IMPLICIT NONE
    
    ! Declare variables
    INTEGER :: i
    REAL :: real_val
    DOUBLE PRECISION :: double_val

    ! Converting an integer 
    i = 5
    PRINT *, "Integer value: ", i
    PRINT *, "Converted to REAL: ", REAL(i)
    PRINT *, "Converted to DOUBLE PRECISION: ", DBLE(i)   

    ! Converting a real 
    real_val = 12.75
    PRINT *, "Real value: ", real_val
    PRINT *, "Converted to INTEGER (truncated): ", INT(real_val) !(truncating the decimal part)
    PRINT *, "Converted to DOUBLE PRECISION: ", DBLE(real_val)

    ! Converting a double precision 
    double_val = 1.75D0  ! The D0 indicates that this is a double precision literal
    PRINT *, "Double precision value: ", double_val
    PRINT *, "Converted to REAL: ", REAL(double_val)            
    PRINT *, "Converted to INTEGER (truncated): ", INT(double_val) !  (truncating the decimal part)

END PROGRAM type_conversion