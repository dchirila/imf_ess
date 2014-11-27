! File: function_with_automatic_array.f90
! Purpose: Demonstrate how to use automatic arrays inside procedures.

function countingSort( inArray, k )
  implicit none
  integer, dimension(:), intent(in) :: inArray
  integer, intent(in) :: k ! max possible element-value in 'inArray'
  integer, dimension( size(inArray) ) :: countingSort ! func result
  ! NOTE: automatic array (shape depends on function's arguments)
  integer :: workArray(0:k)
  integer :: i

  ! Automatic arrays cannot be initialized on declaration-line
  workArray = 0
  ! Place histogram of input inside workArray.
  do i=1, size(inArray)
     workArray( inArray(i) ) = workArray( inArray(i) ) + 1
  end do
  ! Accumulate in workArray(i) the # of elements less than or
  ! equal to i.
  do i=1, k
     workArray(i) = workArray(i) + workArray(i-1)
  end do
  ! Place elements at appropriate position in output-array.
  do i=size( inArray ), 1, -1
     countingSort( workArray(inArray(i)) ) = inArray(i)
     workArray( inArray(i) ) = workArray( inArray(i) ) - 1
  end do
end function countingSort

program function_with_automatic_array
  implicit none
  interface
     function countingSort( inArray, k )
       integer, dimension(:), intent(in) :: inArray
       integer, intent(in) :: k
       integer, dimension( size(inArray) ) :: countingSort
     end function countingSort
  end interface
  integer, parameter :: N = 10
  integer :: testArray(N) = [ 10, 7, 4, 8, 9, 5, 6, 7, 6, 4 ]
  character(len=40) outFmtArray

  ! create dynamically the format-string for displaying the array (to avoid
  ! hard-coding array size inside the format-string)
  write(outFmtArray,'(a,i0,a)') "(", N, "(i0, 2x))"

  write(*,*) "testArray (original):"
  write(*, fmt=outFmtArray) testArray

  write(*,*) "countingSort(testArray): "
  write(*, fmt=outFmtArray) countingSort(testArray, N)
end program function_with_automatic_array

