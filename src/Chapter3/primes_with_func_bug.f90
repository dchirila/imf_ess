! File: primes_with_func_bug.f90
! Purpose: Demonstrate how easily software-bugs can be introduced, when
!          procedure-interfaces are implicit (here, because of passing a 'real'
!          to a function which expects an 'integer'-argument).
! NOTE: Compare with file 'primes_with_func_bug_avoided.f90', where we use one
!       of the mechanisms for making the interface *explicit*, allowing the
!       compiler to prevent this kind of bugs.

logical function isPrimeFunc1a( nr )
  implicit none
  ! data-declarations (for interface)
  integer, intent(in) :: nr
  ! data-declarations (local variables)
  integer :: i, squareRoot

  ! for DEBUGGING: print the number we received
  !write(*, '(a,1x,i0)') "isPrimeFunc1d: got nr =", nr

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

program primes_with_func_bug
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n

  ! with the old declaration (implicit interface), the compiler cannot catch
  ! the bug...
  logical isPrimeFunc1a

  do n=2, N_MAX
     !                  bug
     if(isPrimeFunc1a(n*1.0)) print*, n
  end do
end program primes_with_func_bug
