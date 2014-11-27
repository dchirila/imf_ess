! File: function_assumed_shape_array.f90
! Purpose: Demonstrate how to declare and call a function with assumed-shape
!          dummy arrays. Two versions are presented:
!          - V2: time-bounds information is lost inside the function
!          - V3: time-bounds information is maintained inside the function,
!          because the lower time-bound is passed as an additional argument

real function calcAvgTempV2( inArray )
  implicit none
  ! assumed-shape dummy array
  real, dimension(:,:), intent(in) :: inArray ! Celsius

  calcAvgTempV2 = sum( inArray, mask=(inArray > -273.15) ) / &
       count( mask=(inArray > -273.15) )
end function calcAvgTempV2

real function calcAvgTempV3( inArray, startTime )
  implicit none
  ! explicit lower-bound, to preserve array-shape
  integer, intent(in) :: startTime
  ! assumed-shape dummy array, with explicit lower-bound
  real, dimension(startTime:, :), intent(in) :: inArray ! Celsius

  calcAvgTempV3 = sum( inArray, mask=(inArray > -273.15) ) / &
       count( mask=(inArray > -273.15) )
end function calcAvgTempV3

program function_assumed_shape_array
  implicit none
  interface
     ! explicit interface for V2
     real function calcAvgTempV2( inArray )
       real, dimension(:,:), intent(in) :: inArray
     end function calcAvgTempV2

     ! explicit interface for V3
     real function calcAvgTempV3( inArray, startTime )
       integer, intent(in) :: startTime
       real, dimension(startTime:,:), intent(in) :: inArray
     end function calcAvgTempV3
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
                                ! un-comment one of the calls below
                                !calcAvgTempV2( testData )
       calcAvgTempV3( testData, START_TIME )
end program function_assumed_shape_array

