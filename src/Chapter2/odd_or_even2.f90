program odd_or_even2
  implicit none
  integer inputNum

  print*, "Enter an integer number:"
  read*, inputNum

  select case( mod(inputNum, 2) )
  case(0)
     print*, inputNum, " is even."
  case default
     print*, inputNum, " is odd."
  end select

end program odd_or_even2
