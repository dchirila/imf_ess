! File: save_attribute_debugging_counters.f90
! Purpose: Demonstrate how saved-variables can be used for debugging purposes;
!          in the present case, the variable 'evenCountDebug' tracks the number
!          of times the subroutine was called, with an even number passed for
!          the 'inNumber' dummy-argument.
! NOTE: Debugging information should not appear during normal executions of the
!       program. One way to achieve this is by surrounding the
!       debugging-specific code by preprocessor directives (check the
!       documentation of your compiler for details).

subroutine compute4thPower( inNumber, outNumber )
  implicit none
  integer, intent(in) :: inNumber
  integer, intent(out) :: outNumber
  integer, save :: evenCountDebug=0
  if( mod(inNumber, 2) == 0 ) then
     evenCountDebug = evenCountDebug + 1
  end if
  write(*,'(a,i0,/)') "@ compute4thPower: evenCountDebug = ", evenCountDebug

  ! code for subroutine's functionality (not much in this example)
  outNumber = inNumber**4
end subroutine compute4thPower

program save_attribute_debugging_counters
  implicit none
  interface
     subroutine compute4thPower( inNumber, outNumber )
       integer, intent(in) :: inNumber
       integer, intent(out) :: outNumber
     end subroutine compute4thPower
  end interface
  integer :: nIn, nOut

  do nIn=1, 10
     write(*,'(a,1x,i0)') "passing nIn =", nIn
     call compute4thPower( nIn, nOut )
  end do
end program save_attribute_debugging_counters

