program number_is_odd4
  implicit none
  integer :: inputNum

  write(*, '(a)', advance="no") "Enter an integer number: "
  read(*, *) inputNum
  ! if which includes both "else if"- and else-branches
  if( mod(inputNum, 2) /= 0 ) then
     write(*, '(i0, a)') inputNum, " is odd"
     ! inputNum is odd, now test if it is zero
  else if( inputNum == 0 ) then
     write(*, '(i0, a)') inputNum, " is zero"
     ! default, "catch-all" branch, if all tests fail
  else
     write(*, '(i0, a)') inputNum, " is non-zero and even"
  end if
end program number_is_odd4


