PROGRAM IO_File_Read
    ! ********************************************
    ! READ DATA FROM FILE
    ! ********************************************
    implicit none

    ! Variable declarations
    integer :: i, file_ID, io_status
    integer :: unit_number      ! for file creation
    integer :: num_digits
    integer :: line_skip
    integer :: n                ! for each line
    double precision :: x1, x2, x3, x4 ! for each line
    character(len=50) :: filename
    character(len=10) :: formatted_number
    character(len=20) :: format_string
    character(len=20) :: filename_prefix
    character(len=4) :: filename_suffix

    ! Build the filename. Example: output_0027.vtk
    filename_prefix = 'output_'
    filename_suffix = '.vtk'
    num_digits = 4              ! Number of digits
    file_ID = 27

    ! Build format string
    write(format_string, '(A,I0,A,I0,A)') '(I', num_digits, '.', num_digits, ')'

    ! Format file_ID
    write(formatted_number, trim(format_string)) file_ID

    ! Concatenate strings to form the full filename
    filename = trim(adjustl(filename_prefix)) // trim(adjustl(formatted_number)) // filename_suffix

    ! Print the filename for debugging
    print *, "Reading from file:", filename

    ! Assign a unit number for file reading (unit number must be unique)
    unit_number = 10

    ! Open the file for reading
    open(unit=unit_number, file=filename, status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        print *, "Error opening file:", filename
        stop
    endif

    ! Skip the first 5 lines
    line_skip = 5
    do i = 1, line_skip
        read(unit_number, *, iostat=io_status) ! Reading and discarding the lines
        if (io_status /= 0) then
            print *, "Error or end of file while skipping lines"
            stop
        endif
    end do

    ! Reading loop: one integer and four double precision numbers per line
    do
        read(unit_number, *, iostat=io_status) n, x1, x2, x3, x4
        if (io_status /= 0) exit ! Exit loop if there's an error (e.g., end of file)
        ! Print the values read from the file
        print *, "Integer:", n, " Doubles:", x1, x2, x3, x4
    end do

    ! Close the file after reading
    close(unit_number)
END PROGRAM IO_File_Read

