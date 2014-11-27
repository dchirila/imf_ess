! File: test_proc_fort_v1.f90
! Purpose: Code for caled procedure, for application 'demo_c_v1'
! Author: Dragos B. Chirila

subroutine test_proc_fort_v1() &
     bind(C, name='test_proc_fort_v1')
  write(*,'(a)') 'Hello from "test_proc_fort_v1(Fort)",&
       &invoked from demo_c_v1(C)"!'
end subroutine test_proc_fort_v1
