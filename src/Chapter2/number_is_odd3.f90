program number_is_odd3
  implicit none
  integer :: inputNum

  write(*, '(a)', advance="no") "Enter an integer number: "
  read(*, *) inputNum
  ! if which includes an else-branch also
  if( mod(inputNum, 2) /= 0 ) then
     write(*, '(i0, a)') inputNum, " is odd"
  else
     write(*, '(i0, a)') inputNum, " is even"
  end if
end program number_is_odd3


