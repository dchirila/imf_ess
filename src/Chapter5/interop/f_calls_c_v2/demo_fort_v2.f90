! File: demo_fort_v2.f90
! Purpose: Main-program for application 'demo_fort_v2'
! Author: Dragos B. Chirila

program demo_fort_v2
  use iso_c_binding, only: c_int, c_double
  implicit none

  integer :: i, j ! dummy indices
  integer(c_int) :: n_fort = 17
  real(c_double), dimension(2,3) :: arr_fort

  ! IFACE to C-function.
  interface
     subroutine test_proc_c_v2(n_c, arr_c) &
          bind(C, name='test_proc_c_v2')
       use iso_c_binding, only: c_int, c_double
       integer(c_int), intent(in), value :: n_c
       real(c_double), dimension(2,3), intent(in) :: arr_c
     end subroutine test_proc_c_v2
  end interface

  ! initialize 'arr_fort' with some data
  do j=1,3
     do i=1,2
        arr_fort(i,j) = real(i*j, c_double)
     end do
  end do

  call test_proc_c_v2(n_fort, arr_fort)  ! Fort -call-> C
end program demo_fort_v2
