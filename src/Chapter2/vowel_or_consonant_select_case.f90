program vowel_or_consonant_select_case
  implicit none
  character :: letter

  write(*, '(a)', advance="no") &
       "Type a letter of the English alphabet: "
  read(*, '(a1)') letter

  select case( letter )
  case ('a', 'e', 'i', 'o', 'u', &
       'A', 'E', 'I', 'O', 'U')
     write(*, '(4a)') '"', letter, '"', " is a vowel"
     ! note below: match-list consists of values,
     ! as well as value-ranges
  case ('b':'d', 'f', 'g', 'h', 'j':'n', 'p':'t', 'v':'z', &
       'B':'D', 'F', 'G', 'H', 'J':'N', 'P':'T', 'V':'Z')
     write(*, '(4a)') '"', letter, '"', " is a consonant"
  case default
     write(*, '(4a)') '"', letter, '"', " is not a letter!"
  end select
end program vowel_or_consonant_select_case
