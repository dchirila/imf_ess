! File: sample_surface_optional_args.f90
! Purpose: Demonstrate optional procedure-arguments, by augmenting the program
!          'sample_surface.f90'. This new solution makes the 'res' and
!          'outFileName' arguments optional.

subroutine sampleFunctionToFileV2( xMin, xMax, yMin, yMax, res, outFileName )
  implicit none
  real, intent(in) :: xMin, xMax, yMin, yMax
  integer, optional, intent(in) :: res
  character(len=*), optional, intent(in) :: outFileName
  integer :: i, j, outFileID
  real :: x, y, a, b, c, d
  ! default values for optional arguments
  integer, parameter :: defaultRes = 300
  character(len=*), parameter :: defaultOutFileName = "test_func_sample.dat"
  ! local vars for optional-args
  integer :: actualRes
  character(len=256) :: actualOutFileName ! need to specify length

  ! initialize local vars corresponding to optional-args. If the caller
  ! actually provided values for these args, we copy them; otherwise, we use
  ! the default values...
  ! ...res
  if( present(res) ) then
     actualRes = res
  else
     actualRes = defaultRes
  endif
  ! ...outFileName
  if( present(outFileName) ) then
     actualOutFileName = outFileName
  else
     actualOutFileName = defaultOutFileName
  endif

  ! ensure 'actualRes' value is valid (should be >=2)
  if( actualRes < 2 ) then
     write(*,'(a,1x,i0,1x,a)') "Error: res =", res, "is invalid! Aborting."; stop
  end if

  ! open output-file
  open(newunit=outFileID, file=trim(adjustl(actualOutFileName)), status="replace")
  ! evaluate scaling-coefficients
  a=(xMax-xMin)/(actualRes-1); b=(actualRes*xMin-xMax)/(actualRes-1)
  c=(yMax-yMin)/(actualRes-1); d=(actualRes*yMin-yMax)/(actualRes-1)

  do i=1, actualRes
     do j=1, actualRes
        x = a*i+b; y = c*j+d ! scale to real
        write(outFileID, '(3(f16.8))') x, y, cos( x*(x+y) )*exp( -0.05*(x**2+y**2) )
     end do
     write(outFileID,*) ! newline for GnuPlot
  end do
  close(outFileID)
end subroutine sampleFunctionToFileV2

program sample_surface_optional_args
  implicit none
  interface
     subroutine sampleFunctionToFileV2( xMin, xMax, yMin, yMax, res, &
          outFileName )
       real, intent(in) :: xMin, xMax, yMin, yMax
       integer, optional, intent(in) :: res
       character(len=*), optional, intent(in) :: outFileName
     end subroutine sampleFunctionToFileV2
  end interface

  ! call which does not specify the res-argument
  call sampleFunctionToFileV2( xMin=-5., xMax=5., yMin=-10., yMax=10., &
       outFileName="test_func_sample_lowres.dat" )
end program sample_surface_optional_args
