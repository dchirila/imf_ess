program season_select_case
  implicit none
  character(len=30) :: line

  write(*, '(a)', advance="no") "Enter 3-letter season acronym: "
  read(*, '(a)') line

  if( len_trim(line) == 3 ) then
     season_match: select case( trim(line) )
     case ("djf", "DJF") season_match
        write(*, '(a)') "Season is: winter"
     case ("mam", "MAM") season_match
        write(*, '(a)') "Season is: spring"
     case ("jja", "JJA") season_match
        write(*, '(a)') "Season is: summer"
     case ("son", "SON") season_match
        write(*, '(a)') "Season is: autumn"
     case default season_match
        write(*, '(5a)') &
             '"', trim(line), '"', " is not a valid acronym", &
             " for a season!"
     end select season_match
  else
     write(*, '(5a)') &
          '"', trim(line), '"', " is cannot be a valid acronym", &
          " for a season, because it does not have 3 characters!"
  end if

end program season_select_case

