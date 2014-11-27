! File: function_assumed_length_string.f90
! Purpose: Demonstrate how to pass strings of assumed-length to a function, and
!          how such a function may be called.

integer function countVowels( strng )
  implicit none
  character(len=*), intent(in) :: strng
  integer :: numVowels, i

  numVowels = 0 ! reset counter
  ! it is allowed to inquire the length of the actual-argument with 'len'
  do i=1, len(strng)
     select case( strng(i:i) )
     case( 'a', 'e', 'i', 'o', 'u', &
          'A', 'E', 'I', 'O', 'U' )
        numVowels = numVowels + 1
     end select
  end do
  countVowels = numVowels
end function countVowels

program function_assumed_length_string
  implicit none
  interface
     integer function countVowels( strng )
       character(len=*), intent(in) :: strng
     end function countVowels
  end interface

  write(*, '(a, i0)') "countVowels('Here is a string') = ", &
       countVowels("Here is a string")
  write(*, '(a, i0)') "countVowels('And also another one!') = ", &
       countVowels("And also another one!")
end program function_assumed_length_string

