PROGRAM Create_a_movie

    implicit none
    integer, parameter :: n = 100
    real :: x(n), y(n)
    integer :: i, j

    CHARACTER(LEN=:), ALLOCATABLE :: data_filename, script_filename, plot_filename, plot_filename_template, &
        gif_filename
    CHARACTER(LEN=3) :: j_as_string
    CHARACTER(LEN=:), ALLOCATABLE :: command 

    ! CHARACTER(LEN=:), ALLOCATABLE :: data_filename, script_filename, plot_filename, plot_filename_template, &
	! gif_filename, plot_filename_prefix, plot_filename_suffix
	! CHARACTER(LEN=:), ALLOCATABLE :: gnuplot_path, image_magick_path
    ! CHARACTER(LEN=3) :: j_as_string
    ! CHARACTER(LEN=:), ALLOCATABLE :: command 
    
	! gnuplot_path = 'C:\Program Files\gnuplot\bin\gnuplot.exe'   					! Useful for Windows only
	! image_magick_path = 'C:\Program Files\ImageMagick-7.1.1-Q16-HDRI\magick.exe'   ! Useful for Windows machine only
	
    ! Generate 5 images
    DO j = 1, 10         
        ! Each plot contains n=100 points
        DO i = 1, n
            ! Generate (x,y) data for (i,j) given
            x(i) = (i - 1) * 2.0 * 3.14159 / (n - 1)  ! x from 0 to 2*pi
            y(i) = sin(x(i) + j * 2 * 3.1415 / 10)
        END DO
            
        ! Save (x,y) data to file. Example: 'sinus_001.dat', 'sinus_002.dat', ...
        WRITE (j_as_string, '(I3.3)') j
        data_filename = 'sinus_' // j_as_string // '.dat'
        OPEN (unit=10, file=data_filename, status='replace')
        DO i = 1, n
            WRITE (10, *) x(i), y(i)
        END DO
        CLOSE (10)    
        
        ! Write a gnuplot script, with one flavor out of two 
        ! Save to file directly (no popup window to display plot)
        plot_filename = 'sinus_' // j_as_string // '.png'
        script_filename = 'script.gp'     
        OPEN (unit=20, file = script_filename, status = 'replace')
        WRITE (20, '(a)') 'set terminal png'
        WRITE (20, '(a)') "set output '" // plot_filename // "'" 
        WRITE (20, '(a)') 'set title "This is my title"'
        WRITE (20, '(a)') 'set xlabel "x"'
        WRITE (20, '(a)') 'set ylabel "y"'
        WRITE (20, '(a,i2,a)') 'plot "' // data_filename // '" using 1:2 with lines'
        WRITE (20, '(a)') 'q'    
        CLOSE (20)  
                
		! Run the Gnuplot script for Windows machine
		! command =  '"' // gnuplot_path // '" ' // script_filename // ''			! Comment out if on Linux machine		    
		! CALL execute_command_line(command)										! Comment out if on Linux machine
	
		! Run the Gnuplot script for Linux machine
        ! command = 'gnuplot ' // script_filename
		CALL system ('gnuplot ' // script_filename)								! Comment out if on Windows machine

    END DO

    ! Define the command to convert PNG images into a GIF (delay in milliseconds)
    plot_filename_template = 'sinus_*.png'
    gif_filename = 'film.gif'
	
	! Run the ImageMagick(=Convert) script for Windows machine
	! command = '"' // image_magick_path // '" -delay 10 -loop 0 ' &
    !   // plot_filename_prefix // '*' // plot_filename_suffix // ' ' & 
    !   // gif_filename
    ! CALL execute_command_line(command)
	
	! Run the ImageMagick(=Convert) script for Linux machine
	CALL system('convert -delay 20 -loop 0 ' // plot_filename_template // ' ' // gif_filename)
        
END PROGRAM Create_a_movie