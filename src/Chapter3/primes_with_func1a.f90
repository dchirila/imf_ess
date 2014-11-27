! File: primes_with_func1a.f90
! Purpose: Simple example, demonstrating how to declare and use a function.

logical function isPrimeFunc1a( nr )
  implicit none
  ! data-declarations (for interface)
  integer, intent(in) :: nr
  ! data-declarations (local variables)
  integer :: i, squareRoot

  if( nr <= 1 ) then
     isPrimeFunc1a = .false.; return
  elseif( nr == 2 ) then
     isPrimeFunc1a = .true.; return
  elseif( mod(nr, 2) == 0) then
     isPrimeFunc1a = .false.; return
  else
     squareRoot = int( sqrt(real(nr)) )
     do i=3, squareRoot, 2
        if( mod(nr, i) == 0 ) then
           isPrimeFunc1a = .false.; return
        endif
     enddo
  endif
  isPrimeFunc1a = .true.
end function isPrimeFunc1a

program primes_with_func1a
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n
  ! declaration for function
  ! NOT the recommended approach
  logical isPrimeFunc1a

  do n=2, N_MAX
     if(isPrimeFunc1a(n)) print*, n
  enddo
end program primes_with_func1a
