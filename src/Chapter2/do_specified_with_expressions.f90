program do_specified_with_expressions
  implicit none
  integer :: timeMax = 10, step = 1, i, numLoopTrips = 0

  do i=1, timeMax, step
     timeMax = timeMax / 2
     step = step * 2
     numLoopTrips = numLoopTrips + 1
     write(*, '(a, i0, a, /, 3(a, i0, /))') &
          "Loop body executed ", numLoopTrips, " times", &
          "i = ", i, &
          "timeMax = ", timeMax, &
          "step = ", step
  end do

end program do_specified_with_expressions
