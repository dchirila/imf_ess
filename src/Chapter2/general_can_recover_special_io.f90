program general_can_recover_special_io
  implicit none
  integer :: anInteger

  ! special forms, default formatting...
  read  *, anInteger                  ! input
  print *, "You entered: ", anInteger ! output

  ! ...equivalent general forms, default formatting
  read (*, *) anInteger                  ! input
  write(*, *) "You entered: ", anInteger ! output

  ! special forms, custom formatting...
  read  '(i20)', anInteger                 ! input
  print '("You entered: ", i0)', anInteger ! output

  ! ...equivalent general forms, custom formatting
  read  (*,'(i20)') anInteger                 ! input
  write (*,'("You entered: ", i0)') anInteger ! output
end program general_can_recover_special_io

