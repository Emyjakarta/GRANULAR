PROGRAM math_functions

    implicit none

    ! Variable declarations
    real :: a, c, result
    real :: b
    real :: angle, pi
    real :: exp_val, log_val, log10_val, sin_val, cos_val, tan_val
    real :: max_val, min_val, abs_val
    real :: x, y, fractional_part
    double precision :: num, exponent, power_val
    integer :: int1, int2, int_mod, integer_part
    real :: sign_val

    ! Initialize variables
    a = 25.0
    b = 10.3   
    c = -5.0
    pi = 3.141592653589793

    ! Square root
    result = sqrt(a)

    ! Exponential and logarithm (natural log)
    exp_val = exp(b)
    log_val = log(b)
    ! Compute the base 10 logarithm and power
    log10_val = log10(b)
    num = 2.3d0             ! double precision --> d0 suffix
    exponent = -4.1d0       ! double precision --> d0 suffix
    power_val = num**exponent  ! Or use: power_val = power(num, exponent)
    
    ! Trigonometric functions (angle in radians)
    angle = pi / 4.0  ! 45 degrees in radians
    sin_val = sin(angle)
    cos_val = cos(angle)
    tan_val = tan(angle)

    ! Absolute value
    abs_val = abs(c)

    ! Integer and fractional parts
    integer_part = int(b)
    fractional_part = b - integer_part

    ! Maximum and minimum
    max_val = max(a, b)
    min_val = min(a, b)

    ! Sign function (returns the magnitude of the first argument and the sign of the second)
    x = 7.0
    y = -3.0
    sign_val = sign(x, y)
    print *, "sign(", x, ", ", y, ") = ", sign_val  ! Should return -7.0 (sign from y)

    ! Integer modulus (the two arguments must be integers)
    int1 = 17
    int2 = 5
    int_mod = mod(int1, int2)
    print *, "Integer modulus of ", int1, " divided by ", int2, " is: ", int_mod

    ! Rounding, ceiling, flooring
    num = pi
    print *, "Ceiling (integer) of ", num, " is: ", ceiling(num)
    print *, "Rounding (integer) of ", num, " is: ", nint(num)
    print *, "Rounding (real) of ", num, " is: ", anint(num) 
    print *, "Floor (integer) of ", num, " is: ", floor(num)



END PROGRAM math_functions 