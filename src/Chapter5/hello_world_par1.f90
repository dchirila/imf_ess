program hello_world_par1
  implicit none

  !$omp parallel
    print*, "Hello, world of Modern Fortran! Parallel too!"
  !$omp end parallel
end program hello_world_par1
