! File: primes_with_sub.f90
! Purpose: Simple example, demonstrating how to declare and use a subroutine.

subroutine isPrimeSub( nr, isPrime )
  implicit none
  ! data-declarations (for interface)
  integer, intent(in) :: nr
  logical, intent(out) :: isPrime
  ! data-declarations (local variables)
  integer :: i, squareRoot

  if( nr <= 1 ) then
     isPrime = .false.; return
  elseif( nr == 2 ) then
     isPrime = .true.; return
  elseif( mod(nr, 2) == 0) then
     isPrime = .false.; return
  else
     squareRoot = int( sqrt(real(nr)) )
     do i=3, squareRoot, 2
        if( mod(nr, i) == 0 ) then
           isPrime = .false.; return
        endif
     enddo
  endif
  isPrime = .true.
end subroutine isPrimeSub

program primes_with_sub
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n
  logical :: stat

  do n=2, N_MAX
     call isPrimeSub(n, stat)
     if(stat) print*, n
  enddo
end program primes_with_sub
