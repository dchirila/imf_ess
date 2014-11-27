program birthday_day_of_week
  implicit none
  character(len=20) :: name
  integer :: birthDate(3), year, month, day, dayOfWeek
  integer, dimension(12) :: t = &
       [ 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 ]

  print*, "Enter name (inside apostrophes/quotes):"
  read*, name
  print*, "Now, enter your birth date (year, month, day):"
  read*, birthDate

  year = birthDate(1); month = birthDate(2); day = birthDate(3)

  if( month < 3 ) then
     year = year - 1
  end if

  ! Formula of Tomohiko Sakamoto (1993)
  ! Interpretation of result: Sunday = 0, Monday = 1, ...
  dayOfWeek = &
       mod( (year + year/4 - year/100 + year/400 + t(month) + day), 7)

  print*, name, " was born on a "
  select case(dayOfWeek)
  case(0)
     print*, "Sunday"
  case(1)
     print*, "Monday"
  case(2)
     print*, "Tuesday"
  case(3)
     print*, "Wednesday"
  case(4)
     print*, "Thursday"
  case(5)
     print*, "Friday"
  case(6)
     print*, "Saturday"
  end select

end program birthday_day_of_week

