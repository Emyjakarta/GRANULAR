MODULE modul_utils

    IMPLICIT NONE

  CONTAINS
  
    FUNCTION generate_filename(file_ID, f_prefix, f_suffix) RESULT(f_full)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: file_ID						! Input
      CHARACTER(LEN=*), INTENT(IN) :: f_prefix, f_suffix	! Input
      CHARACTER(LEN=:), ALLOCATABLE :: f_full				! Output
      CHARACTER(LEN=:), ALLOCATABLE :: f_tempo				! Local
      CHARACTER(LEN=6) :: formatted_number					! Local
      INTEGER :: trimmed_length								! Local
  
      ! Format file_ID with zero padding to 6 digits
      WRITE(formatted_number, '(I6.6)') file_ID
      
      ! Concatenate strings, trimming trailing spaces from each part
      f_tempo = f_prefix // formatted_number // f_suffix
      
      ! Allocate filename to the exact length without trailing spaces
      trimmed_length = LEN_TRIM(f_tempo)
      ALLOCATE(CHARACTER(LEN=trimmed_length) :: f_full)
      f_full = f_tempo
    END FUNCTION generate_filename
  
  END MODULE modul_utils