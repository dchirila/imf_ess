program demo_omp_single
  use omp_lib
  implicit none
  integer :: nThreads

  !$omp parallel
    !$omp single
      nThreads = omp_get_num_threads()
      write(*,'(2(a,x,i0,x),a)') "Thread", omp_get_thread_num(), &
        "says: team has", nThreads, "threads"
    !$omp end single

    ! remaining code within parallel-region executed by all
    write(*,'(a,x,i0,x,a)') "Thread", omp_get_thread_num(), &
      "says: executing common code!"
  !$omp end parallel
end program demo_omp_single
