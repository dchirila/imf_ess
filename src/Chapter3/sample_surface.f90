! File: sample_surface.f90
! Purpose: Demonstrate how keywords can be used, to pass arguments to a
!          procedure in a different order than declared in the
!          procedure-definition. Keywords are useful for avoiding bugs, as well
!          as for making code more readable, if the dummy-arguments in the
!          procedure-definition have been given sensible names.

subroutine sampleFunctionToFileV1( xMin, xMax, yMin, yMax, res, outFileName )
  implicit none
  real, intent(in) :: xMin, xMax, yMin, yMax
  integer, intent(in) :: res
  character(len=*), intent(in) :: outFileName
  integer :: i, j, outFileID
  real :: x, y, a, b, c, d

  ! ensure 'res' received a valid value (should be >=2)
  if( res < 2 ) then
     write(*,'(a,1x,i0,1x,a)') "Error: res =", res, "is invalid! Aborting."; stop
  end if
  open(newunit=outFileID, file=outFileName, status="replace")
  ! evaluate scaling-coefficients
  a = (xMax-xMin)/(res-1); b = (res*xMin-xMax)/(res-1)
  c = (yMax-yMin)/(res-1); d = (res*yMin-yMax)/(res-1)

  do i=1, res
     do j=1, res
        x = a*i+b; y = c*j+d ! scale to real
        write(outFileID, '(3(f16.8))') x, y, cos( x*(x+y) )*exp( -0.05*(x**2+y**2) )
     end do
     write(outFileID,*) ! newline for GnuPlot
  end do
  close(outFileID)
end subroutine sampleFunctionToFileV1

program sample_surface
  implicit none
  interface
     subroutine sampleFunctionToFileV1( xMin, xMax, yMin, yMax, res, &
          outFileName )
       real, intent(in) :: xMin, xMax, yMin, yMax
       integer, intent(in) :: res
       character(len=*), intent(in) :: outFileName
     end subroutine sampleFunctionToFileV1
  end interface

  ! call where the arguments are in a different order than declared in the
  ! function's argument-list
  call sampleFunctionToFileV1( outFileName="test_func_sample.dat", &
       xMin=-5., yMin=-5., xMax=10., yMax=10., res=200 )
end program sample_surface
