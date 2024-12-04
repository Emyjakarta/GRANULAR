PROGRAM Gnuplot

    implicit none
    integer, parameter :: n = 100
    real :: x(n), y(n)
    integer :: i, output_style   
    CHARACTER(LEN=:), ALLOCATABLE :: data_filename, script_filename, plot_filename, gnuplot_path
    CHARACTER(LEN=:), ALLOCATABLE :: command 

    data_filename = 'sinus.dat'
    script_filename = 'script.gp'
    plot_filename = 'image.png'
	! gnuplot_path = 'C:\Program Files\gnuplot\bin\gnuplot.exe'

    
    
    ! Generate (x,y) data
    DO i = 1, n
        x(i) = (i - 1) * 2.0 * 3.14159 / (n - 1)  ! x from 0 to 2*pi
        y(i) = sin(x(i))
    END DO
        
    ! Save (x,y) data to file 'sinus.dat'
    OPEN(unit=10, file=data_filename, status='replace')
    DO i = 1, n
        WRITE(10, *) x(i), y(i)
    END DO
    CLOSE(10)    
    
    ! Write a gnuplot script, with one flavor out of two 
    output_style = 2
    SELECT CASE (output_style)
        CASE (1)        
            ! Open a popup window and display plot until RETURN is pressed. Then save to file.
            OPEN (unit=20, file = script_filename, status = 'replace')
            WRITE (20, '(a)') 'set title "This is my title"'
            WRITE (20, '(a)') 'set xlabel "x"'
            WRITE (20, '(a)') 'set ylabel "y"'
            WRITE (20, '(a,i2,a)') 'plot "' // data_filename // '" using 1:2 with lines'
            WRITE (20, '(a)') 'pause -1'            
            WRITE (20, '(a)') 'q'    
            CLOSE (20)  
        CASE (2)        
            ! Save to file directly (no popup window to display plot)
            OPEN (unit=20, file = script_filename, status = 'replace')
            WRITE (20, '(a)') 'set terminal png'
            WRITE (20, '(a)') "set output '" // plot_filename // "'" 
            WRITE (20, '(a)') 'set title "This is my title"'
            WRITE (20, '(a)') 'set xlabel "x"'
            WRITE (20, '(a)') 'set ylabel "y"'
            WRITE (20, '(a,i2,a)') 'plot "' // data_filename // '" using 1:2 with lines'
            WRITE (20, '(a)') 'q'    
            CLOSE (20)  
    END SELECT
          
    ! Run the Gnuplot script for Windows machine
    ! command =  '"' // gnuplot_path // '" ' // script_filename // ''			! Comment out if on Linux machine		    
	! CALL execute_command_line(command)										! Comment out if on Linux machine
	
	! Run the Gnuplot script for Linux machine
	! CALL system ('gnuplot ' // script_filename)								! Comment out if on Windows machine

    command = 'gnuplot ' // script_filename
    CALL execute_command_line(command)

END PROGRAM Gnuplot