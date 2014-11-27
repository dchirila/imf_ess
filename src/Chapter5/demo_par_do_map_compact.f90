program demo_par_do_map_compact
  implicit none
  real, dimension(:), allocatable :: arr
  real :: step = 0.1
  integer, parameter :: I4B = selected_int_kind(18)
  integer(I4B) :: numElems = 1E7, i

  arr = [ (i*step, i=1, numElems) ]

  !$omp parallel do
    do i=1, numElems
      arr(i) = sin( arr(i) )
    end do
  !$omp end parallel do

  write(*,*) arr(numElems)
end program demo_par_do_map_compact
