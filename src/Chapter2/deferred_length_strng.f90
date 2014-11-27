program deferred_length_strng
  implicit none

  character(len=256) :: buffer  ! fixed-length buffer
  character(len=:), allocatable :: filename  ! deferred-length string

  print*, 'Please enter filename (less than 256 characters):'
  read*, buffer  ! place user-input into fixed buffer

  filename = &  ! copy from buffer to dynamic-size string
       trim(adjustl(buffer))  ! 'trin' and 'adjustl' exaplained later

  print*, filename  ! some feedback...
end program deferred_length_strng
