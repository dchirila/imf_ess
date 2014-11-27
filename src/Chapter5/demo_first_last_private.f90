program demo_first_last_private
  use omp_lib
  implicit none
  integer :: x=1, y=2, z=3, i=4

  write(*,'(4(a,i0))') "A (serial) :: x = ", x, &
    ", y = ", y, ", z = ", z, ", i = ", i

  write(*,*) ! output-separator

  !$omp parallel private(x) shared(y) &
  !$omp firstprivate(z)
    write(*,'(5(a,i0))') &
      "B (parallel) :: Thread ", omp_get_thread_num(), &
      " says: x = ", x, ", y = ", y, ", z = ", z, &
      ", i = ", i
    ! assign to private variable
    x = 2*omp_get_thread_num()

    write(*,'(5(a,i0))') &
      "C (parallel) :: Thread ", omp_get_thread_num(), &
      " says: x = ", x, ", y = ", y, ", z = ", z, &
      ", i = ", i
  !$omp end parallel

  write(*,*) ! output-separator

  !$omp parallel do shared(y)
    do i=1, 42
      y = y+i ! *** BUG! *** (data-race)
    end do
  !$omp end parallel do

  write(*,'(4(a,i0))') "D (serial) :: x = ", x, &
    ", y = ", y, ", z = ", z, ", i = ", i

  !$omp parallel sections lastprivate(i)
    !$omp section
      i = 11
    !$omp section
      i = 22
    !$omp section
      i = 33
  !$omp end parallel sections

  write(*,'(4(a,i0))') "E (serial) :: x = ", x, &
    ", y = ", y, ", z = ", z, ", i = ", i
end program demo_first_last_private
