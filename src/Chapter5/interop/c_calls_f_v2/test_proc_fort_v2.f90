! File: test_proc_fort_v2.f90
! Purpose: Code for caled procedure, for application 'demo_c_v2'
! Author: Dragos B. Chirila

subroutine test_proc_fort_v2(n, arr) &
     bind(C, name='test_proc_fort_v2')
  use iso_c_binding, only: c_int, c_double
  integer(c_int), intent(in), value :: n
  real(c_double), dimension(2,3), intent(in) :: arr
  integer :: i, j ! dummy indices

  write(*,'(a)') 'Hello from "test_proc_fort_v2(Fort)",&
       &invoked from demo_c_v2(C)"!'
  write(*,'(a,i0)') "n = ", n
  do j=1,3
     do i=1,2
        write(*,'(2(a,i0),a,f8.2)') "arr[", i, ",", j, "] = ", arr(i,j)
     end do
  end do
end subroutine test_proc_fort_v2
