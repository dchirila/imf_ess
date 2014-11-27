! File: primes_with_func_bug_avoided.f90
! Purpose: Demonstrate how explicit procedure-interfaces (here, achieved with an
!          interface-block) allows the compiler to help the programmer to
!          identify a class of bugs (passing data of the wrong type to a
!          procedure).
! NOTES: 1. The compilation for this file should *FAIL*, which is a good thing
!        (much better than having programs which silently compute the incorrect
!        result).
!        2. Having interface-blocks written directly into the body of the
!        calling (sub)program, as shown here, is just one way of making the
!        callee-interface explicit (but not the most convenient way often --
!        modules help in this respect).

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

program primes_with_func_bug_avoided
  implicit none
  integer, parameter :: N_MAX=100
  integer :: n

  ! If we make the interface explicit (e.g. with the
  ! interface-block below), the bug is easily identified.
  interface
     logical function isPrimeFunc1a( nr )
       integer, intent(in) :: nr
     end function isPrimeFunc1a
  end interface

  do n=2, N_MAX
     !                  bug
     if(isPrimeFunc1a(n*1.0)) print*, n
  end do
end program primes_with_func_bug_avoided
