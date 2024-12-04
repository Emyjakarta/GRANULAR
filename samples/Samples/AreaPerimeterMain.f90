PROGRAM AreaPerimeterMain

    USE RectangleModule  ! Use the module containing functions and subroutines
    IMPLICIT NONE

    ! Declaration of local variables
    REAL :: a, b

    ! Initialize values
    a = 5.0
    b = 3.0

    ! Call the subroutine to calculate the area and perimeter
    CALL CalculateAreaAndPerimeter(a, b)

    ! Print the results
    PRINT *, "The area of the rectangle is: ", area
    PRINT *, "The perimeter of the rectangle is: ", perimeter

END PROGRAM AreaPerimeterMain