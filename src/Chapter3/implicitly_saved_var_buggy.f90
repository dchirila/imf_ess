! File: implicitly_saved_var_buggy.f90
! Purpose: Demonstrate how procedure-variables, when they are initialized on the
!          declaration line, silently acquire the 'save' attribute.

real function sumArrayElementsV1( inArray )
  implicit none
  real, dimension(:), intent(in) :: inArray
  real :: tmpSum=0.0
  integer :: i

  do i=1, size(inArray)
     tmpSum = tmpSum + inArray(i)
  end do
  sumArrayElementsV1 = tmpSum ! collect result
end function sumArrayElementsV1

program implicitly_saved_var_buggy
  implicit none
  interface
     real function sumArrayElementsV1( inArray )
       real, dimension(:), intent(in) :: inArray
     end function sumArrayElementsV1
  end interface
  real, dimension(4) :: testArray = [ 1.0, 3.0, 2.1, 7.9 ]

  write(*,*) "sum1 = ", sumArrayElementsV1( testArray )
  write(*,*) "sum2 = ", sumArrayElementsV1( testArray )
end program implicitly_saved_var_buggy

