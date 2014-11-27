program hello_world_par2
  implicit none
  integer :: nThreads

  write(*, '(a)', advance='no') "nThreads="
  read*, nThreads

  !$omp parallel num_threads(nThreads)
    print*, "Hello, world of Modern Fortran! Parallel too!"
  !$omp end parallel
end program hello_world_par2
