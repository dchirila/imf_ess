! File: OceanData_class.f90
! Purpose: Implementation for the OceanData "class"; this should be able to
!          read the annual data from:
!          http://www.nodc.noaa.gov/OC5/WOA09/netcdf_data.html
!
! NOTE: can ignore lines with '! .....'
! Author: Dragos B. Chirila

module OceanData_class
  use netcdf
  use NumericKinds
  use GeomUtils
  implicit none

  type, public :: OceanData
     private
     ! dimension-lengths
     integer :: mNumLon, mNumLat, mNumDepth
     ! arrays to hold data
     real(R_SP), dimension(:), allocatable :: mLonVals, mLatVals, mDepthVals
     real(R_SP), dimension(:,:), allocatable :: mLonBndsVals, mLatBndsVals
     real(R_SP), dimension(:,:,:), allocatable :: mDataVals
     ! additional metadata
     real(R_SP) :: mDataFillValue
   contains
     private
     procedure, public :: getDepths
     procedure, public :: getMeanDepthProfile
     ! internal
     procedure :: cellHasValidData
     procedure :: getCellArea
  end type OceanData

  interface OceanData
     module procedure newOceanData
  end interface OceanData

contains
  ! ...............

  type(OceanData) function newOceanData( fileName, dataFieldName ) result(res)
    character(len=*), intent(in) :: fileName, dataFieldName
    ! local vars
    integer :: ncID, lonDimID, latDimID, depthDimID, lonVarID, latVarID, &
         depthVarID, dataVarID, lonBndsVarID, latBndsVarID

    call ncCheck( nf90_open(path=fileName, mode=NF90_NOWRITE, ncid=ncID) )

    ! Read-in Dimensions:
    ! (A) retrieve dimension-IDs
    call ncCheck( nf90_inq_dimid(ncID, name="lon", dimid=lonDimID) )
    call ncCheck( nf90_inq_dimid(ncID, name="lat", dimid=latDimID) )
    call ncCheck( nf90_inq_dimid(ncID, name="depth", dimid=depthDimID) )
    ! (B) read dimension-lengths
    call ncCheck( nf90_inquire_dimension(ncID, lonDimID, len=res%mNumLon) )
    call ncCheck( nf90_inquire_dimension(ncID, latDimID, len=res%mNumLat) )
    call ncCheck( nf90_inquire_dimension(ncID, depthDimID, len=res%mNumDepth) )

    ! Can allocate memory, now that dimension-lengths are known
    allocate( res%mLonVals(res%mNumLon), res%mLatVals(res%mNumLat) )
    allocate( res%mDepthVals(res%mNumDepth), res%mLonBndsVals(2,res%mNumLon) )
    allocate( res%mLatBndsVals(2,res%mNumLat) )
    allocate( res%mDataVals(res%mNumLon,res%mNumLat,res%mNumDepth) )

    ! Read-in Dimension-Variables:
    ! (A) retrieve variable-IDs
    call ncCheck( nf90_inq_varid(ncID, "lon", lonVarID) )
    call ncCheck( nf90_inq_varid(ncID, "lat", latVarID) )
    call ncCheck( nf90_inq_varid(ncID, "depth", depthVarID) )
    ! (B) read variable-arrays
    call ncCheck( nf90_get_var(ncID, lonVarID, res%mLonVals) )
    call ncCheck( nf90_get_var(ncID, latVarID, res%mLatVals) )
    call ncCheck( nf90_get_var(ncID, depthVarID, res%mDepthVals) )

    ! Read-in Bounds-Variables (for lon/lat)
    ! (A) retrieve variable-IDs
    call ncCheck( nf90_inq_varid(ncID, "lon_bnds", lonBndsVarID) )
    call ncCheck( nf90_inq_varid(ncID, "lat_bnds", latBndsVarID) )
    ! (B) read variable-arrays (here, 2D arrays)
    call ncCheck( nf90_get_var(ncID, lonBndsVarID, res%mLonBndsVals) )
    call ncCheck( nf90_get_var(ncID, latBndsVarID, res%mLatBndsVals) )

    ! Read-in data-field-Variable (and associated attribute "_FillValue")
    call ncCheck( nf90_inq_varid(ncID, trim(adjustl(dataFieldName)), dataVarID) )
    call ncCheck( nf90_get_att(ncID, dataVarID, "_FillValue", &
         res%mDataFillValue) )
    call ncCheck( nf90_get_var(ncID, dataVarID, res%mDataVals) )

    call ncCheck( nf90_close(ncID) )
  end function newOceanData

  function getDepths( this ) result(res)
    class(OceanData), intent(in) :: this
    real(R_SP), dimension(:), allocatable :: res

    res = this%mDepthVals
  end function getDepths

  ! NOTE: depth does not need to be used here, since the radius of the Earth is
  !       so much larger.
  function getCellArea( this, idxLon, idxLat ) result(res)
    class(OceanData), intent(in) :: this
    integer, intent(in) :: idxLon, idxLat
    real(RK) :: res
    ! local variables
    real(RK), parameter :: R_E = 6.371E6_RK
    real(RK) :: lonW, lonE, latS, latN

    lonW = real(this%mLonBndsVals(1,idxLon),RK)
    lonE = real(this%mLonBndsVals(2,idxLon),RK)
    latS = real(this%mLatBndsVals(1,idxLat),RK)
    latN = real(this%mLatBndsVals(2,idxLat),RK)

    res = R_E**2 * (deg2rad(lonE)-deg2rad(lonW)) * &
         (sinD(latN)-sinD(latS))
  end function getCellArea


  function getMeanDepthProfile( this ) result(res)
    class(OceanData), intent(in) :: this
    real(R_QP), dimension(:), allocatable :: res
    ! local variables
    ! use higher-precision for the result (to avoid possible overflow during the
    ! intermediate stages)
    real(R_QP) :: areaTimesValue, levelArea, dArea
    integer :: idxLon, idxLat, idxDepth

    allocate( res(this%mNumDepth) )

    do idxDepth=1, this%mNumDepth
       areaTimesValue=0._R_QP; levelArea=0._R_QP; dArea=0._R_QP
       do idxLat=1, this%mNumLat
          do idxLon=1, this%mNumLon
             if( this%cellHasValidData(idxLon, idxLat, idxDepth) ) then
                dArea = this%getCellArea(idxLon, idxLat)
                levelArea = levelArea + dArea
                areaTimesValue = areaTimesValue + &
                     this%mDataVals(idxLon, idxLat, idxDepth)*dArea
             end if
          end do
       end do
       res(idxDepth) = areaTimesValue/levelArea
    end do
  end function getMeanDepthProfile

  logical function cellHasValidData( this, idxLon, idxLat, idxDepth )
    class(OceanData), intent(in) :: this
    integer, intent(in) :: idxLon, idxLat, idxDepth

    cellHasValidData = &
         (this%mDataVals(idxLon,idxLat,idxDepth) /= this%mDataFillValue)
    !abs(this%mDataVals(idxLon,idxLat,idxDepth)-this%mDataFillValue) > &
    !2*epsilon(this%mDataFillValue)
  end function cellHasValidData

  ! error-checking wrapper for netCDF operations
  subroutine ncCheck( status )
    integer(I3B), intent(in) :: status

    if( status /= nf90_noerr ) then
       write(*,'(a)') trim( nf90_strerror(status) )
       stop "ERROR (netCDF-related) See message above for details! Aborting..."
    end if
  end subroutine ncCheck
end module OceanData_class
