! File: implicitly_saved_var_fixed.f90
! Purpose: Corrected version of program 'implicitly_saved_var_buggy.f90'. In
!          this version, the procedure-local variable 'tmpSum' is initialized
!          outside the declaration-line, so that it does NOT acquire the
!          'save'-attribute.

real function sumArrayElementsV2( inArray )
  implicit none
  real, dimension(:), intent(in) :: inArray
  real :: tmpSum
  integer :: i

  tmpSum = 0.0 ! NOTE: variable-initialization now in executable section.

  do i=1, size(inArray)
     tmpSum = tmpSum + inArray(i)
  end do
  sumArrayElementsV2 = tmpSum ! collect result
end function sumArrayElementsV2

program implicitly_saved_var_buggy
  implicit none
  interface
     real function sumArrayElementsV2( inArray )
       real, dimension(:), intent(in) :: inArray
     end function sumArrayElementsV2
  end interface
  real, dimension(4) :: testArray = [ 1.0, 3.0, 2.1, 7.9 ]

  write(*,*) "sum1 = ", sumArrayElementsV2( testArray )
  write(*,*) "sum2 = ", sumArrayElementsV2( testArray )
end program implicitly_saved_var_buggy


