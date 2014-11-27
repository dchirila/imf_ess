program greeting_the_user
  implicit none
  character(len=20) :: userName
  integer :: userAge

  print*, "Enter your name (inside apostrophes/quotes) &
       &and age: "
  read*, userName, userAge
  print*, "Hello, ", userName, &
       ", you will soon be ", userAge+1, " years old!"
end program greeting_the_user

