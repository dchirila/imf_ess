! File: reading_cli_arguments.f90
! Purpose: Demonstrate the two methods of reading command-line arguments.
! NOTES: - Try to run the program while providing various combinations of
!          command-line arguments, e.g.:
!          $ ./reading_cli_arguments
!          $ ./reading_cli_arguments --help
!          $ ./reading_cli_arguments --n 100
!        - Of course, any other names can be chosen instead of 'nargs',
!        'string_len', and 'cmd_stat'

program reading_cli_arguments
  implicit none
  integer :: nargs, string_len, cmd_stat, i
  character(len=100) :: string_val

  ! Method 1: Get the entire command-line invocation.
  call get_command( command=string_val, length=string_len, status=cmd_stat)
  write(*,'(2a, 2(a,i0))') "complete command-line was: '", trim(string_val), &
       "' | length was: ", string_len, " | status was: ", cmd_stat

  ! Method 2: Get the individual command-line arguments.
  nargs = command_argument_count()
  write(*,'(a,1x,i0,1x,a)') "received", nargs, "command-line arguments"

  do i=0, nargs
     call get_command_argument(i, value=string_val, length=string_len, &
          status=cmd_stat )
     write(*,'(a,i0,3a)') "argument #", i, " = '", trim(string_val), "'"
  end do
end program reading_cli_arguments
