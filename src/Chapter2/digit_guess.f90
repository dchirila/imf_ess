program digit_guess
  implicit none
  integer :: try=5
  character :: userInput='u' ! Initialize with invalid
  logical :: guessed=.false.

  print*, "Pick a digit between 0 and 9. I will guess it!"

  do
     ! termination condition
     if( guessed .eqv. .true. ) exit
     print*, "Is it ", try, "? (y for Yes | n for No)"
     read*, userInput
     select case(userInput)
     case('y')
        guessed = .true.
     case('n')
        print*, "Is it larger or smaller? &
             &(l for larger | s for smaller)"

        do
           read*, userInput
           select case(userInput)
           case('s')
              try = try - 1
              exit
           case('l')
              try = try + 1
              exit
           case default
              print*, "Invalid response. Trying again..."
           end select
        end do

     case default
        print*, "Invalid response. Trying again..."
     end select
  end do

  print*, "Finally got it! Bye..."
end program digit_guess

