! File: rng_estimate_pi.f90
! Purpose: Simple Monte-Carlo simulation, for demonstrating how to work with the
!          intrinsic Random Number Generator (RNG) in Fortran; here, this is
!          applied to the classical test-problem of estimating the value of
!          number PI.

program rng_estimate_pi
  implicit none
  integer, parameter :: NUM_DRAWS_TOTAL=1e7
  integer :: countDrawsInCircle=0, i
  real :: randomPosition(2)
  integer :: seedArray(16)

  ! quick method to fill seedArray
  call date_and_time(values=seedArray(1:8))
  call date_and_time(values=seedArray(9:16))
  print*, seedArray
  ! seed the RNG
  call random_seed(put=seedArray)

  do i=1, NUM_DRAWS_TOTAL
     call random_number( randomPosition )
     if( (randomPosition(1)**2 + randomPosition(2)**2) < 1.0 ) then
        countDrawsInCircle = countDrawsInCircle + 1
     end if
  end do
  print*, "estimated pi =", &
       4.0*( real(countDrawsInCircle) / real(NUM_DRAWS_TOTAL))
end program rng_estimate_pi
