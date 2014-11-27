program season_many_nested_ifs
  implicit none
  character(len=30) :: line

  write(*, '(a)', advance="no") "Enter 3-letter season acronym: "
  read(*, '(a)') line

  if( len_trim(line) == 3 ) then
     winter: if( trim(line) == "djf" ) then
        write(*, '(a)') "Season is: winter"
     else if( trim(line) == "DJF" ) then winter
        write(*, '(a)') "Season is: winter"
     else winter
        spring: if( trim(line) == "mam" ) then
           write(*, '(a)') "Season is: spring"
        else if( trim(line) == "MAM" ) then spring
           write(*, '(a)') "Season is: spring"
        else spring
           summer: if( trim(line) == "jja" ) then
              write(*, '(a)') "Season is: summer"
           else if( trim(line) == "JJA" ) then summer
              write(*, '(a)') "Season is: summer"
           else summer
              autumn: if( trim(line) == "son" ) then
                 write(*, '(a)') "Season is: autumn"
              else if( trim(line) == "SON" ) then autumn
                 write(*, '(a)') "Season is: autumn"
              else autumn
                 write(*, '(5a)') &
                      '"', trim(line), '"', " is not a valid acronym", &
                      " for a season!"
              end if autumn
           end if summer
        end if spring
     end if winter
  else
     write(*, '(5a)') &
          '"', trim(line), '"', " is cannot be a valid acronym", &
          " for a season, because it does not have 3 characters!"
  end if
end program season_many_nested_ifs
