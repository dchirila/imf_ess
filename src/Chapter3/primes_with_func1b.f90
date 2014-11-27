! File: primes_with_func1b.f90
! Purpose: Simple example, demonstrating how to declare and use a function; this
!          version moves the function-result declaration inside the body of the
!          function (it was in the header-line for primes_with_func1a.f90).

function isPrimeFunc1b( nr )
  implicit none
  ! data-declarations (for interface)
  integer, intent(in) :: nr
  logical :: isPrimeFunc1b ! NOTE: return-type defined here; no 'intent'
  !       allowed (it is effectively "out")
  ! data-declarations (local variables)
  integer :: i, squareRoot

  if( nr <= 1 ) then
     isPrimeFunc1b = .false.; return
  elseif( nr == 2 ) then
     isPrimeFunc1b = .true.; return
  elseif( mod(nr, 2) == 0) then
     isPrimeFunc1b = .false.; return
  else
     squareRoot = int( sqrt(real(nr)) )
     do i=3, squareRoot, 2
        if( mod(nr, i) == 0 ) then
           isPrimeFunc1b = .false.; return
        endif
     enddo
  endif
  isPrimeFunc1b = .true.
end function isPrimeFunc1b

program primes_with_func1b
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n
  ! declaration for function
  ! NOT the recommended approach
  logical isPrimeFunc1b

  do n=2, N_MAX
     if(isPrimeFunc1b(n)) print*, n
  enddo
end program primes_with_func1b
