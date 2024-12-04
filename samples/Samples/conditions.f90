PROGRAM conditions

    implicit none

    ! Variable declarations
    integer :: age, score


    ! Initialize variables
    age = 14
    score = 0

    ! Select case  
    select case (score)
    case (0)
        print *, "Really so bad ?"
    case (1:59)
        print *, "You have received an F (Fail)."
    case (60:99)
        print *, "You have received a D, a C or a B or a A."
    case (100)
        print *, "You are genius"
    case default
        print *, "Invalid score. Please enter a score between 0 and 100."
    end select

    ! If - then - else 
    if (age < 0) then
        print *, "Age cannot be negative. Please enter a valid age."
    else if (age < 20) then
        print *, "You are a child or a teenager."
    else if (age < 65) then
        print *, "You are an adult."
    else
        print *, "You are a senior citizen."
    end if

    ! Logical operators
    ! 
    ! .AND. (Logical AND)    
    ! .OR.  (Logical OR)
    ! .NOT. (Logical NOT)

    if (score > 4 .AND. score <= 7) then
        print *, "The score (", score, ") is 5, 6 or 7" 
    else
        print *, "The score (", score, ") is not 5, 6 or 7"
    end if

    if (.NOT. (score == 0)) then
        print *, "The score is not zero."
    else
        print *, "The score is zero."
    end if

END PROGRAM conditions