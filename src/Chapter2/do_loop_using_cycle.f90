program do_loop_using_cycle
  implicit none
  integer, parameter :: SEC_IN_MIN = 60, &
       SEC_IN_HOUR = 60*SEC_IN_MIN, & ! 60 minutes in hour
       SEC_IN_DAY = 24*SEC_IN_HOUR, & ! 24 hours in a day
       SEC_IN_WEEK = 7*SEC_IN_DAY ! 7 days in a week
  integer :: secIn, weeks, days, hours, minutes, sec

  do
     write(*, '(/, a)', advance="no") & ! '/' adds newline, for separation
          "Enter number of seconds (or 0 to exit the program): "
     read(*, *) secIn

     if( secIn == 0 ) then ! loop-termination criterion
        exit
     else if( secIn < 0 ) then ! skipping criterion
        write(*, '(a)') "Error: number of seconds should be" // &
             " positive. Try again!"
        cycle ! ** calculation skipped with CYCLE **
     end if

     ! calculation using the value
     sec = secIn ! backup value
     weeks = sec / SEC_IN_WEEK;  sec = mod(sec, SEC_IN_WEEK)
     days = sec / SEC_IN_DAY;    sec = mod(sec, SEC_IN_DAY)
     hours = sec / SEC_IN_HOUR;  sec = mod(sec, SEC_IN_HOUR)
     minutes = sec / SEC_IN_MIN; sec = mod(sec, SEC_IN_MIN)
     ! display final hierarchy
     write(*, '(6(i0, a))') secIn, "s = { ", &
          weeks, " weeks, ", days, " days, ", &
          hours, " hours, ", minutes, " minutes, ", &
          sec, " seconds }"
  end do
end program do_loop_using_cycle

