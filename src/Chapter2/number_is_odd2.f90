program number_is_odd2
  implicit none
  integer :: inputNum

  write(*, '(a)', advance="no") "Enter an integer number: "
  read(*, *) inputNum
  ! compact version of the previous if-test
  if( mod(inputNum, 2) /= 0 ) write(*, '(i0, a)') inputNum, " is odd"
end program number_is_odd2

