! File: demo_namelist.f90
! Purpose: Demonstrate how to work with namelists, i.e.:
!          - how to define a namelist-group
!          - how to use the namelist-group to write/read data.

program demo_namelist
  implicit none
  ! user-defined DT
  type GeoLocation
     real :: mLon, mLat
  end type GeoLocation

  ! Variable-declarations
  logical :: flag = .false.
  integer :: inFileID=0, outFileID=0
  real :: threshold = 0.8
  real, dimension(10) :: array = 4.8
  type(GeoLocation) :: myPos = GeoLocation(8.81, 53.08)

  ! namelist-group (binds variables together, for namelist I/O).
  namelist/my_namelist/ flag, threshold, array, myPos

  ! Write some values in a section of the array
  call random_number( array(::3) ) ! section-stride = 3

  ! Write current data-values to a namelist-file
  open(newunit=outFileID, file="demo_namelist_write.nml")
  write(outFileID, nml=my_namelist)
  close(outFileID)

  ! Update (read) *some* values in the namelist, from another file
  open(newunit=inFileID, file="demo_namelist_read.nml")
  read(inFileID, nml=my_namelist)
  close(inFileID)

  ! Display the new program-state
  write(*,'(a)') "After update, the data is:"
  write(*, nml=my_namelist)
end program demo_namelist
