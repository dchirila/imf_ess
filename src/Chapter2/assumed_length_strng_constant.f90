program assumed_length_strng_constant
  implicit none

  character(len=*), parameter :: FILENAME = &  ! character constant
       'really_long_file_name_for_which_&
       &we_do_not_want_to_count_characters.out'

  print*, 'string length:', len(FILENAME)
end program assumed_length_strng_constant
