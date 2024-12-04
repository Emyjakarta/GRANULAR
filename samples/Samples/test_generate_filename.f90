PROGRAM test_generate_filename
    USE modul_utils
    IMPLICIT NONE
    
    ! Declare inputs as allocatable strings for flexibility
    CHARACTER(LEN=:), ALLOCATABLE :: prefix, suffix, full_filename
    INTEGER :: file_id
  
    ! Assign values
    prefix = "output/data_"
    suffix = ".txt"
    file_id = 42

    ! Call function to generate filename
    full_filename = generate_filename(file_id, prefix, suffix)
  
    ! Print the generated filename
    PRINT *, "Generated filename: '", full_filename, "'"
  
  END PROGRAM test_generate_filename