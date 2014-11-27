! File: primes_with_func.f90
! Purpose: Simple example, demonstrating how to declare and use a function; this
!          version demonstrates how to declare the returned dummy-variable with
!          'result' (most often used to give it a different name from that of
!          the function itself).

! NOTE: function-result renamed to 'primStat'
function isPrimeFunc1c( nr )  result(primStat)
  implicit none
  ! data-declarations (for interface)
  integer, intent(in) :: nr
  logical :: primStat ! NOTE: return-type declared here
  ! data-declarations (local variables)
  integer :: i, squareRoot

  if( nr <= 1 ) then
     primStat = .false.; return
  elseif( nr == 2 ) then
     primStat = .true.; return
  elseif( mod(nr, 2) == 0) then
     primStat = .false.; return
  else
     squareRoot = int( sqrt(real(nr)) )
     do i=3, squareRoot, 2
        if( mod(nr, i) == 0 ) then
           primStat = .false.; return
        endif
     enddo
  endif
  primStat = .true.
end function isPrimeFunc1c

program primes_with_func1c
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n
  ! declaration for function
  ! NOT the recommended approach
  logical isPrimeFunc1c

  do n=2, N_MAX
     if(isPrimeFunc1c(n)) print*, n
  enddo
end program primes_with_func1c
