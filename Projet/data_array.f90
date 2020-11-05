program array


    !Generates the arrays of data array of the 
    implicit none

    !Declaration
    integer, parameter :: N = 10
    real, dimension(N) :: x,y
    integer            :: i
    !Main
    
    do i = 1, N
        
        if ( i > 3 .and. i < 6 ) then ! Examples
            
            x(i) = 1     ! Data_Needed
            y(i) = 1     ! Data_Needed
            
            else
            
                x(i) = 0
                y(i) = 0

        end if

    end do

    print *,x
    print *,y

end program array