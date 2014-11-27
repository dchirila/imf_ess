subroutine doTaskA()
  implicit none
  write(*,'(a)') "Working hard on task A!"
end subroutine doTaskA

subroutine doTaskB()
  implicit none
  write(*,'(a)') "Working hard on task B!"
end subroutine doTaskB

program demo_par_sections
  implicit none

  !$omp parallel num_threads(2)
    !$omp sections
      !$omp section
        call doTaskA()
      !$omp section
        call doTaskB()
    !$omp end sections
  !$omp end parallel
end program demo_par_sections
