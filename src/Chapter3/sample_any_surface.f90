! File: sample_any_surface.f90
! Purpose: Demonstrate how to pass functions as procedure-arguments, to make the
!          function-sampling code more useful.

subroutine sampleFunctionToFileV3( xMin, xMax, yMin, yMax, func, outFileName )
  implicit none
  real, intent(in) :: xMin, xMax, yMin, yMax
  ! IFACE for procedure-argument
  interface
     real function func( x, y )
       real, intent(in) :: x, y
     end function func
  end interface
  character(len=*), intent(in) :: outFileName
  integer :: i, j, outFileID
  real :: x, y, a, b, c, d
  integer :: res = 300

  open(newunit=outFileID, file=outFileName, status="replace")
  ! evaluate scaling-coefficients
  a=(xMax-xMin)/(res-1); b=(res*xMin-xMax)/(res-1)
  c=(yMax-yMin)/(res-1); d=(res*yMin-yMax)/(res-1)

  do i=1, res
     do j=1, res
        x = a*i+b; y = c*j+d ! scale to real
        write(outFileID, '(3(f16.8))') x, y, func( x, y )
     end do
     write(outFileID,*)
  end do
  close(outFileID)
end subroutine sampleFunctionToFileV3

module TestFunctions2D
contains
  real function evalFunc1( x, y )
    real, intent(in) :: x, y
    evalFunc1 = cos( x*(x+y) )*exp( -0.05*(x**2+y**2) )
  end function evalFunc1

  real function evalFunc2( x, y )
    real, intent(in) :: x, y
    evalFunc2 = cos( x+y )
  end function evalFunc2
end module TestFunctions2D

program sample_any_surface
  use TestFunctions2D
  implicit none
  interface
     subroutine sampleFunctionToFileV3( xMin, xMax, yMin, yMax, func, &
          outFileName )
       real, intent(in) :: xMin, xMax, yMin, yMax
       interface
          real function func( x, y )
            real, intent(in) :: x, y
          end function func
       end interface
       character(len=*), intent(in) :: outFileName
     end subroutine sampleFunctionToFileV3
  end interface

  ! sample function 1
  call sampleFunctionToFileV3( xMin=-5., xMax=5., yMin=-10., yMax=10., &
       func=evalFunc1, outFileName="sampling_func1.dat" )

  ! sample function 2
  call sampleFunctionToFileV3( xMin=-5., xMax=5., yMin=-10., yMax=10., &
       func=evalFunc2, outFileName="sampling_func2.dat" )
end program sample_any_surface
