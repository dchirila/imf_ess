program odd_or_even
  implicit none
  integer inputNum

  print*, "Enter an integer number:"
  read*, inputNum
  ! NOTE: mod is an intrinsic function, returning
  ! remainder of division of two integers.
  if( mod(inputNum, 2) .eq. 0 ) then
     print*, inputNum, " is even."
  else
     print*, inputNum, " is odd."
  end if
end program odd_or_even
