MODULE RectangleModule
    IMPLICIT NONE

    ! Declaration of global variables
    REAL :: area, perimeter

CONTAINS

    ! Function to calculate the area of a rectangle
    REAL FUNCTION CalculateArea(length, width)
        IMPLICIT NONE
        REAL :: length, width
        CalculateArea = length * width  ! Calculate area
    END FUNCTION CalculateArea

    ! Subroutine to calculate area and perimeter of a rectangle
    SUBROUTINE CalculateAreaAndPerimeter(len, wid)
        IMPLICIT NONE
        REAL :: len, wid  ! Define local variables
        area = CalculateArea(len, wid)  ! Call the function to get area
        perimeter = 2.0 * (len + wid)    ! Calculate perimeter
    END SUBROUTINE CalculateAreaAndPerimeter

END MODULE RectangleModule