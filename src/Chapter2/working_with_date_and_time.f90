program working_with_date_and_time
  implicit none
  ! for date_and_time-call
  integer :: dateAndTimeArray(8)
  ! for cpu_time-call
  real :: timeStart, timeEnd
  ! variables for expensive loop
  integer :: mySum=0, i

  call date_and_time(values=dateAndTimeArray)
  print*, "dateAndTimeArray =", dateAndTimeArray

  call cpu_time(time=timeStart)
  ! expensive loop
  do i=1, 1000000000
     mySum = mySum + mySum/i
  end do
  call cpu_time(time=timeEnd)
  print*, "Time for expensive loop =", timeEnd-timeStart, "seconds",&
       ", mySum =", mySum
end program working_with_date_and_time
