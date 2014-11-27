! File: demo_fort_v1.f90
! Purpose: Main-program for application 'demo_fort_v1'
! Author: Dragos B. Chirila

program demo_fort_v1
  implicit none
  ! IFACE to C-function.
  interface
     subroutine test_proc_c_v1() &
          bind(C, name='test_proc_c_v1')
     end subroutine test_proc_c_v1
  end interface

  call test_proc_c_v1()  ! Fort -call-> C
end program demo_fort_v1
