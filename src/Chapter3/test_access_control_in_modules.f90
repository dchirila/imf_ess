! File: test_access_control_in_modules.f90
! Purpose: Demonstrate the various attributes that can be used for setting the
!          access-control (visibility and read/write-permissions) for
!          module-entities.
! NOTE: Access-control for an entity in a module does NOT influence what other
!       entities in the *same* module can do with it -- they only become
!       relevant in other program-units, which use the module.

module TestModule
  implicit none
  private ! Change to restrictive default-access.
  integer, public, protected :: countA=0, countB=0
  integer                    :: countC=0

  public executeTaskA, executeTaskB ! Specify public-interface of the module.
contains
  subroutine executeTaskA()
    call executeTaskC()
    countA = countA + 1 ! increment debug counter
  end subroutine executeTaskA

  subroutine executeTaskB()
    countB = countB + 1 ! increment debug counter
  end subroutine executeTaskB

  subroutine executeTaskC()
    countC = countC + 1 ! increment debug counter
  end subroutine executeTaskC
end module TestModule

program test_access_control_in_modules
  use TestModule
  implicit none

  call executeTaskA() ! Some calls
  call executeTaskB() ! to
  call executeTaskA() ! module-subroutines.
  ! Compilation-error if enabled (subroutine not visible, because it is made
  ! 'private' in the module)
  !call executeTaskC()

  ! Display debugging-counters.
  write(*,'(a,1x,i0,1x,a)') '"executeTaskA" was called', countA, 'times'
  write(*,'(a,1x,i0,1x,a)') '"executeTaskB" was called', countB, 'times'
  ! Compilation-error if enabled (module-variable not visible, because it is
  ! made 'private' in the module)
  !write(*,'(a,1x,i0,1x,a)') '"executeTaskC" was called', countC, 'times'
end program test_access_control_in_modules
