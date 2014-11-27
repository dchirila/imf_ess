program read_with_error_recovery
  implicit none
  integer :: statCode=0, x

  ! The safeguarded READ-statement
  read(unit=*, fmt=*, iostat=statCode, err=123, end=124) x
  print'(a, 1x, i0)', "Received number", x
  ! Normal program termination-point, when no exceptions occur
  stop

123 write(*, '(a, 1x, i0)') &
       "READ encountered an ERROR! iostat =", statCode
  ! can insert here code to recover from error, if possible...
  stop
124 write(*, '(a, 1x, i0)') &
       "READ encountered an end-of-file! iostat =", statCode
  ! can insert here code to recover from error, if possible...
  stop
end program read_with_error_recovery
