program check_month_index_select_case_partial_ranges
  implicit none
  integer :: month
  write(*, '(a)', advance="no") "Enter an integer-value: "
  read(*, *) month

  ! check if month is valid month-index, with partial
  ! (semi-open) ranges in a select-case construct
  select case( month )
  case ( :0, 13: )
     write(*, '(a, i0, a)') "error: ", &
          month, " is not a valid month-index"
  case default
     write(*, '(a, i0, a)') "ok: ", month, &
          " is a valid month-index"
  end select
end program check_month_index_select_case_partial_ranges
