program demo_private_allocatable
  use omp_lib
  implicit none
  integer, dimension(:), allocatable :: aVec
  real, dimension(:,:), allocatable :: aMat

  ! WARNING: 'ifort' needs '-assume realloc_lhs' option for the
  !          following assignment to work!
  aVec = [ 1, 2, 3 ] ! automatic allocation

  write(*,'(a,3(i0,x))') "BEFORE: aVec = ", aVec

  !$omp parallel private(aVec, aMat) num_threads(3)
    write(*,'(a,i0,a,l1,a,l1)') "Thread ", omp_get_thread_num(), &
      " says: allocated(aVec) = ", allocated(aVec), &
      ", allocated(aMat) = ", allocated(aMat)

    if( allocated(aVec) ) then
      write(*,'(a,i0,a,i0,a,3(i0,x))') "Thread ", omp_get_thread_num(), &
        " says: size(aVec) = ", size(aVec), &
        ", aVec = ", aVec
    end if
  !$omp end parallel

  write(*,'(a,3(i0,x))') "AFTER: aVec = ", aVec
end program demo_private_allocatable
