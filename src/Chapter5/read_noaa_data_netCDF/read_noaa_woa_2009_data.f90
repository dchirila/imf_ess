! File: read_noaa_woa_2009_data.f90
! Purpose: Simple program, to demonstrate reading-in a dataset with known
!          structure. Particularly, we read the annual-temperature from the NOAA
!          World Ocean Atlas 2009 dataset.
! REFS: Locarnini, R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, H. E.
!       Garcia, O. K. Baranova, M. M. Zweng, and D. R. Johnson, 2010. World
!       Ocean Atlas 2009, Volume 1: Temperature. S. Levitus, Ed. NOAA Atlas
!       NESDIS 68, U.S. Government Printing Office, Washington, D.C., 184 pp.
!
! Author: Dragos B. Chirila


program read_noaa_woa_2009_data
  use OceanData_class
  implicit none

  character(len=*), parameter :: &
       inFileName="temperature_annual_1deg.nc", &
       outFileName="tempDepthProfile.txt"

  real(R_SP), dimension(:), allocatable :: depths
  real(R_QP), dimension(:), allocatable :: meanTemps

  type(OceanData) :: tempAnnual

  integer :: i, outFileID

  tempAnnual = OceanData( inFileName, "t_an" ) ! initialize object

  depths = tempAnnual%getDepths()
  meanTemps = tempAnnual%getMeanDepthProfile()

  ! write depth-temperature pairs (to STDOUT, for simplicity)
  open(newunit=outFileID, file=trim(adjustl(outFileName)), status="replace")
  do i=1, size(depths)
     write(outFileID, '(' // RK_FMT // ',4x,' // RK_FMT // ')') depths(i), meanTemps(i)
  end do
  close(outFileID)
end program read_noaa_woa_2009_data
