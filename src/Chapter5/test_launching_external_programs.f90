! File: test_launching_external_programs.f90
! Purpose: Demonstrate how the "execute_command_line" intrinsic-procedure can be
!          used to launch an external program. Specifically, we launch Gnuplot
!          in this case, to visualize a field (matrix) of randomly-generated
!          values.
! Pre-requisites: Gnuplot should be installed, and its location should be in a
!                 directory which is part of the PATH-variable.
! NOTE: "execute_command_line" (F2008 feature) only works with gfortran at the
! moment (if working with ifort, use the non-standard extension "system").

program test_launching_external_programs
  implicit none
  integer, parameter :: SZ = 50
  real, dimension(SZ, SZ) :: dataMatrix
  integer :: i, j, cmdExitStat, cmdStat, outFileID, plotFileID
  character(len=200) :: cmdMsg

  ! Populate matrix with random-values.
  call random_number( dataMatrix )

  ! Write the values to a file.
  open(newunit=outFileID, file="data_matrix.dat")
  do i=1, SZ
     do j=1, SZ
        write(outFileID, '(2(i0,1x),f0.6,1x)') i, j, dataMatrix(i,j)
     end do
     write(outFileID,*) ! newline for GnuPlot
  end do
  close(outFileID)

  ! Generate a Gnuplot BATCH-script.
  open(newunit=plotFileID,file="visualize_data_matrix.pl")
  write(plotFileID, '(a)') &
       'set size square' &
       , 'unset key' &
       , 'set title "Plot of random matrix"' &
       , 'set pm3d map' &
       , 'set xrange [1:50]' &
       , 'set yrange [1:50]' &
       , 'set palette rgbformulae 22,13,10' &
       , 'splot "./data_matrix.dat" using 1:2:3' &
       , 'pause -1 "Hit <ENTER> to exit visualization"'
  close(plotFileID)

  ! Call Gnuplot, to visualize the result.
  call execute_command_line( &
       "gnuplot visualize_data_matrix.pl", &
       exitstat=cmdExitStat, cmdstat=cmdStat, &
       cmdmsg=cmdMsg )

  ! Display error-reporting info for last command.
  write(*,'(a,1x,i0)') "exitstat =", cmdExitStat
  ! Display error-information only if there was a problem with the
  ! call to 'execute_command_line'.
  if( cmdStat /= 0 ) then
     write(*,'(a,1x,i0)') "cmdstat =", cmdStat
     write(*,'(a,1x,a)') "cmdmsg =", trim(cmdMsg)
  end if

  ! Remove generated files.
  call execute_command_line( "rm data_matrix.dat visualize_data_matrix.pl" )
end program test_launching_external_programs

