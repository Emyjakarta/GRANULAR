PROGRAM functions
    
    IMPLICIT NONE
    
    ! these variables are global: their value is known in the main code and also in the functions body
    real :: initial_temperature, deltaT1, deltaT2
    real :: final_temperature

    ! Initialize values of the global variables
    initial_temperature = 298.0
    deltaT1 = 5.0
    deltaT2 = 3.0
    final_temperature = -1.0        

    ! Perform calculations
    final_temperature = Function1(deltaT1, deltaT2)
    PRINT *, "Final temperature: ", final_temperature
    print *, "Within the main body, we know that initial_temperature=", initial_temperature, " deltaT1=", deltaT1 
    print *, " but C is unknown"

CONTAINS  

    REAL FUNCTION Function1(a, b)       ! one real output, two input parameters
        IMPLICIT NONE
        REAL :: a, b            ! the two input parameters are real
        real :: C
        C = a + b
        Function1 = initial_temperature + C  ! Perform addition
        print *, "Within the function body, we know that initial_temperature=", initial_temperature, " deltaT1=", deltaT1, " C=",C
        print *, "but the final_temperature is not yet updated" 
    END FUNCTION Function1

END PROGRAM functions