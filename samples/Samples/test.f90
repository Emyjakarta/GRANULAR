program My_code
        Implicit none
        character(len=500) ::  Input
        print *,"How are you?"
        Read *,Input
        print *, "I am ", trim(Input)
        END PROGRAM My_code
