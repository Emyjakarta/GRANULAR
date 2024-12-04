PROGRAM IO_Terminal
! Raise error if a variable is not explicitly declared
implicit none

! Variable declarations
integer :: age
real :: size
double precision :: lightspeed
character(len=50) :: name

! Ask multiple simple questions to the user: name, age, size
print *, "What is your name?"
read *, name
print *, "How old are you (must be an integer)?"
read *, age
print *, "What is your size in meters (use point as decimal separator)?"
read *, size

! Print the combined information
print *, "Hello,", trim(name), "! You are", age, "years old and your size is", size, "meters."

! Control the format
! A is used for strings
! I3 formats the integer with a width of 3 (right-aligned)
! F5.2 formats the real number to have a total width of 5 characters, with 2 digits after the decimal point
! E12.5 formats single precision real numbers (8 bits) with total width of 12 characters (including point, E+ and exponent) and 5 digits after the decimal point
! D15.10 formats double precision real numbers (16 bits). Total width 15, 10 decimal places
write (*, '(A, A, A, I3, A, F5.2, A)') "Hello ", trim(name), "! You are ", age, " years old and your size is ", size, " meters."

! Assign numerical value to variables (WARNING: use the suffix d0 for double precision real numbers)
lightspeed = 299792458.123456d0
print *, "Speed of light c = 299792458.123456 is printed as:", lightspeed
write (*, '(A, E17.12)') "or, with a scientific notation: ", lightspeed

END PROGRAM IO_Terminal

