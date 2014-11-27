! File: sample_any_surface_with_error_recovery.f90
! Purpose: More complete version of the function-sampling example, with optional
!          arguments, passing the function to be sampled as a
!          procedure-argument, and also more robust support for error-recovery.
! Error-codes:
! stat =  0 -> no error
! stat = -1 -> invalid 'res'
! stat = -2 -> invalid 'outFileName'
! stat = -3 -> could not open output-file

subroutine sampleFunctionToFileV4( xMin, xMax, yMin, yMax, func, res, &
     outFileName, stat )
  implicit none
  real, intent(in) :: xMin, xMax, yMin, yMax
  ! IFACE for procedure-argument
  interface
     real function func( x, y )
       real, intent(in) :: x, y
     end function func
  end interface
  integer, optional, intent(in) :: res
  character(len=*), optional, intent(in) :: outFileName
  integer, optional, intent(inout) :: stat
  ! local vars
  integer :: i, j, outFileID, ioError
  real :: x, y, a, b, c, d
  ! default values for optional arguments
  integer, parameter :: defaultRes = 300
  character(len=*), parameter :: defaultOutFileName = "test_func_sample.dat"
  ! local vars for optional-args
  integer :: actualRes
  character(len=256) :: actualOutFileName

  ! Initialize local vars corresponding to optional-args (based on actual
  ! call-parameters and/or default values)...
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
  end if

  ! ensure 'actualRes' value is valid (should be >=2)
  if( actualRes < 2 ) then
     if( present(stat) ) then
        ! allow the caller to perform error-recovery
        stat = -1; return
     else
        ! 'stat'-argument not provided, so the only sensible thing to do is crash
        ! the program
        write(*,'(a,1x,i0,1x,a)') "Error: res =", res, "is invalid! Aborting."; stop
     end if
  end if

  ! ensure 'outFileName' value is valid (should be non-empty string)
  ! NOTE: there might also be system-dependent restrictions on valid file-names,
  !       but they are not taken into account here.
  if( len(actualOutFileName) <= 0 ) then
     if( present(stat) ) then
        ! allow the caller to perform error-recovery
        stat = -2; return
     else
        ! only choice is to crash
        write(*,'(a)') "Error: outFileName cannot be an empty! Aborting."; stop
     end if
  end if

  open(newunit=outFileID, file=trim(adjustl(actualOutFileName)), &
       status="replace", iostat=ioError)
  if( ioError /= 0 ) then
     if( present(stat) ) then
        stat = -3; return
     else
        write(*,'(3a)') 'Error: could not OPEN file "', &
             trim(adjustl(actualOutFileName)), '" for writing! Aborting.'; stop
     end if
  end if

  ! Evaluate scaling-coefficients.
  a=(xMax-xMin)/(res-1); b=(res*xMin-xMax)/(res-1)
  c=(yMax-yMin)/(res-1); d=(res*yMin-yMax)/(res-1)

  do i=1, res
     do j=1, res
        x = a*i+b; y = c*j+d ! scale to real
        write(outFileID, '(3(f16.8))') x, y, func( x, y )
     end do
     write(outFileID,*) ! newline for GnuPlot
  end do

  close(outFileID, iostat=ioError)
  if( ioError /= 0 ) then
     if( present(stat) ) then
        stat = -4; return
     else
        write(*,'(3a)') 'Error: could not CLOSE file "', &
             trim(adjustl(actualOutFileName)), '"! Aborting.'; stop
     end if
  end if
end subroutine sampleFunctionToFileV4

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

program sample_any_surface_with_error_recovery
  use TestFunctions2D
  implicit none
  interface
     subroutine sampleFunctionToFileV4( xMin, xMax, yMin, yMax, func, res, outFileName, stat )
       real, intent(in) :: xMin, xMax, yMin, yMax
       interface
          real function func( x, y )
            real, intent(in) :: x, y
          end function func
       end interface
       integer, optional, intent(in) :: res
       integer, optional, intent(inout) :: stat
       character(len=*), optional, intent(in) :: outFileName
     end subroutine sampleFunctionToFileV4
  end interface
  integer :: error

  ! TODO un-comment either (but only one at a time) of the procedure-calls
  ! below, to exercise the different error-recovery mechanisms.

  ! sampling evalFunc1: call should succeed
  !----------------------------------------
  call sampleFunctionToFileV4( xMin=-3., xMax=3., yMin=-3., yMax=3., &
       func=evalFunc1, res=100, outFileName="sampling1_func1.dat", stat=error )
  if( error < 0 ) then
     write(*, '(a,1x,i0,a)') "Error: 'sampleFunctionToFileV4' returned code:", error, ". Aborting..."
     stop
  end if
  !----------------------------------------

  ! sampling evalFunc1: call should FAIL, because of invalid 'res'; because we
  ! are providing an argument for 'stat', the subroutine returns
  ! execution-control to the main-program (recommended practice in production
  ! code)
  !---------------------------------------------------------------------------
  !call sampleFunctionToFileV4( xMin=-3., xMax=3., yMin=-3., yMax=3., &
  !func=evalFunc1, res=0, outFileName="sampling2_func1.dat", stat=error )
  !if( error < 0 ) then
  !write(*, '(a,1x,i0,a)') "Error: 'sampleFunctionToFileV4' returned code:", error, ". Aborting..."
  !stop
  !end if
  !---------------------------------------------------------------------------

  ! sampling evalFunc1: call should FAIL, for the same reason as above (invalid
  ! 'res'); however, in this case, we do not provide an argument for 'stat',
  ! which causes the subroutine to directly terminate the program (not the best
  ! approach, especially for libraries of subroutines)
  !----------------------------------------------------------------------------
  !call sampleFunctionToFileV4( xMin=-3., xMax=3., yMin=-3., yMax=3., &
  !func=evalFunc1, res=0, outFileName="sampling3_func1.dat" )
  !----------------------------------------------------------------------------
end program sample_any_surface_with_error_recovery
