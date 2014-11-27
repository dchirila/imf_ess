program hello_world_par3
  use omp_lib
  implicit none
  integer :: nThreads

  write(*, '(a)', advance='no') "nThreads="
  read*, nThreads

  !$omp parallel num_threads(nThreads)
    if( omp_get_thread_num() == 0 ) then
       write(*, '(a,x,i0,x,a)') "Hello from MASTER (team has", &
            omp_get_num_threads(), "threads)"
    else
       write(*, '(a,x,i0)') "Hello from WORKER number", omp_get_thread_num()
    end if
  !$omp end parallel
end program hello_world_par3
