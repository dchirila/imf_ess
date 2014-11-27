program test_significant_range_real_exponential_output
  implicit none
  real :: testReal

  !! test for e0.2
  ! testReal = 0.0
  ! do
  !    write(*,'(e10.2)') testReal
  !    testReal = testReal + 0.01
  !    if( testReal > 2000.0 ) then
  !       exit
  !    endif
  ! enddo

  ! test for en0.2
  testReal = 0.0
  do
     write(*,'(en12.2)') testReal
     testReal = testReal + 0.0001
     if( testReal > 2000.0 ) then
        exit
     endif
  enddo

  !! test for es0.2
  ! testReal = 0.0
  ! do
  !    write(*,'(es10.2)') testReal
  !    testReal = testReal + 0.01
  !    if( testReal > 2000.0 ) then
  !       exit
  !    endif
  ! enddo
end program test_significant_range_real_exponential_output
