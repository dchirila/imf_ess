subroutine doTaskA()
  implicit none
  write(*,'(a)') "Working hard on task A!"
end subroutine doTaskA

subroutine doTaskB()
  implicit none
  write(*,'(a)') "Working hard on task B!"
end subroutine doTaskB

program demo_par_sections_compact
  implicit none

  !$omp parallel sections num_threads(2)
    !$omp section
      call doTaskA()
    !$omp section
      call doTaskB()
  !$omp end parallel sections
end program demo_par_sections_compact
