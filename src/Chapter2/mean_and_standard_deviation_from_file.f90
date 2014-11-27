! File: mean_and_standard_deviation_from_file.f90
! Purpose: Illustrate how non-deterministic loops can be used; here, the task is
!          to read a simple time-series, and evaluate some simple
!          statistical-moments. Because we do not specify in advance the number
!          of entries in the time-series, we need to use an "infinite"-loop,
!          with a termination-criterion.

program mean_and_standard_deviation_from_file
  implicit none
  integer :: statCode, numVals=0, inFileID
  real :: mean=0.0, variance=0.0, sd=0.0, newValue, &
       sumVals=0.0, sumValsSqr=0.0

  ! open file for reading
  open(newunit=inFileID, file="time_series.dat", action="read")

  ! "infinite" DO-loop, to read an unknown amount of data-values
  data_reading_loop: do
     read(inFileID, *, iostat=statCode) newValue
     ! check if exception was raised during read-operation
     if( statCode /= 0 ) then ! **TERMINATION-CRITERION for DO-loop**
        exit data_reading_loop
     else ! datum read successful
        numVals = numVals + 1
        sumVals = sumVals + newValue
        sumValsSqr = sumValsSqr + newValue**2
     end if
  end do data_reading_loop

  ! close file
  close(inFileID)

  ! evaluate mean (avoiding division by zero, when file is empty)
  if( numVals > 0 ) mean = sumVals / numVals
  ! evaluate 2nd central-moment (variance)
  variance = (sumValsSqr - numVals*mean**2) / (numVals - 1)
  ! evaluate standard-deviation from variance
  sd = sqrt( variance )

  write(*, '(2(a, f10.6))') "mean = ", mean, &
       ", sd = ", sd
end program mean_and_standard_deviation_from_file

