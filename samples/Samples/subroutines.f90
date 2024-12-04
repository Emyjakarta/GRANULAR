PROGRAM subroutines
    
    IMPLICIT NONE
    
    ! declaration of the global variables 
    REAL :: length, width, area, perimeter

    ! Initialize values
    length = 5.0
    width = 3.0

    ! Call the subroutine to calculate the area
    CALL CalculateAreaAndPerimeter(length, width)  

    ! Print the result
    PRINT *, "The area of the rectangle is: ", area
    PRINT *, "The perimeteer of the rectangle is: ", perimeter

CONTAINS   

    ! Subroutine to calculate the area of a rectangle
    SUBROUTINE CalculateAreaAndPerimeter(len, wid)  
        IMPLICIT NONE
        REAL :: len, wid           ! define local  variables
        area = len * wid           ! update global variables
        perimeter = 2*(len + wid)  ! update global variables
    END SUBROUTINE CalculateAreaAndPerimeter

END PROGRAM subroutines