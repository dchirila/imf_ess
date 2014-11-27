program continuation_lines
  implicit none
  integer :: seconds_in_a_day = 0

  ! Normal continuation-lines
  seconds_in_a_day = &
       24*60*60 ! 86400

  print*, seconds_in_a_day

  ! Continuation-lines with a split integer-literal token
  seconds_in_a_day = 2&
       &4*60*60 ! still 86400. In this case, splitting the '24'
                ! is unwise, because it makes code unreadable.
                ! However, for long character strings this can be
                ! useful (see below).
  print*, seconds_in_a_day

  ! Continuation-lines with a split string token.
  print*, "This is a really long string, that norma&
       &lly would not fit on a single line."
end program
