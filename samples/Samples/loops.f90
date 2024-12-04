program loops
    implicit none
    integer :: i, count, score

    ! DO loop that starts with 1 (has a step 1) and ends when the counter is strictly greater than 5
    do i = 1, 5
        write(*, "(I2)", advance="no") i   ! I2 formats i as a 2-digit number
        write(*, "(A)", advance="no") " "  ! Add a space between numbers
    end do
    write(*, "(A)", advance="yes") " "      ! Add a final space and jump to new line

    ! DO loop that starts with 4 (has a step 2) and ends when the counter is strictly greater than 17
    do i=4,17,2
        write(*, "(I2)", advance="no") i   ! I2 formats i as a 2-digit number
        write(*, "(A)", advance="no") " "  ! Add a space between numbers
    end do
    write(*, "(A)", advance="yes") " "      ! Add a final space and jump to new line

    ! DO loop with EXIT condition
    count = 0
    do
        score = count * 2 
        if (score == 12) then
            exit   ! Exit the loop 
        else
            count = count + 1
        end if
    end do
    print *, "Exiting loop with score = ", score, " and count=", count

     ! DO loop with WHILE condition
    count = 0
    do while (count <= 3)
        print *, "Count is: ", count
        count = count + 1
    end do
    print *, "Finished DO WHILE loop for count =", count

end program loops