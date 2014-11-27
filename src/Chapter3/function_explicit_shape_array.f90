! File: function_explicit_shape_array.f90
! Purpose: Demonstrate how to declare and call a function with explicit-shape
!          dummy arrays.

real function calcAvgTempV1( inArray, startTime, endTime, numSites )
  implicit none
  integer, intent(in) :: startTime, endTime, numSites
  ! explicit-shape dummy array
  real, dimension(startTime:endTime, numSites), intent(in) :: inArray ! Celsius

  calcAvgTempV1 = sum( inArray, mask=(inArray > -273.15) ) / &
       count( mask=(inArray > -273.15) )
end function calcAvgTempV1

program function_explicit_shape_array
  implicit none
  interface
     real function calcAvgTempV1( inArray, startTime, endTime, numSites )
       integer, intent(in) :: startTime, endTime, numSites
       real, dimension(startTime:endTime, numSites), intent(in) :: inArray
     end function calcAvgTempV1
  end interface
  integer, parameter :: START_TIME=100, END_TIME=365, N_SITES=20, &
       MISSING_TEMP_MARKER=-9999
  real, dimension(START_TIME:END_TIME, N_SITES) :: testData

  ! insert uniform variates U(0,1).
  call random_number( testData )
  ! scale data to [-20.0 C, 30.0 C] temperature-range.
  testData = testData*50.00 - 20.00

  ! introduce some missing-values
  testData(130, 12) = MISSING_TEMP_MARKER
  testData(263, 17) = MISSING_TEMP_MARKER

  write(*,'(a, f0.4)') "average temperature = ", &
       calcAvgTempV1( testData, START_TIME, END_TIME, N_SITES )
end program function_explicit_shape_array
