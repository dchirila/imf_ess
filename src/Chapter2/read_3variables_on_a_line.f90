program read_3variables_on_a_line
  implicit none
  character(len=100) :: station_name  ! fixed-length, for brevity
  integer :: day_of_year
  real :: temperature

  read*, station_name, day_of_year, temperature
  ! provide feedback (echo input)
  print*, "station_name=", trim(adjustl(station_name)), &
       ", day_of_year=", day_of_year, &
       ", temperature=", temperature
end program read_3variables_on_a_line
