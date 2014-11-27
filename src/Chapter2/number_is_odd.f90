program number_is_odd
  implicit none
  integer :: inputNum

  write(*, '(a)', advance="no") "Enter an integer number: "
  read(*, *) inputNum
  ! NOTE: mod is an intrinsic function, returning the remainder
  ! of dividing first argument by the second one (both integers)
  if( mod(inputNum, 2) /= 0 ) then
     write(*, '(i0, a)') inputNum, " is odd"
  end if
end program number_is_odd

