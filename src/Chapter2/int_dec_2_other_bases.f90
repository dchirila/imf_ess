program int_dec_2_other_bases
  implicit none
  integer :: inputInteger

  ! elements of this will be clarified later
  write(*,'(a)', advance='no') "Enter an integer: "
  ! get number (field width needs to be manually-specified)
  read '(i20)', inputInteger
  ! (string in format discussed later) print...
  print '("binary: ", b0)', inputInteger !...min-width binary
  print '("octal : ", o0)', inputInteger !...min-width octal
  print '("hex   : ", z0)', inputInteger !...min-width hex
end program int_dec_2_other_bases
