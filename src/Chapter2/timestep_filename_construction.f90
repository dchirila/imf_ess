! File: timestep_filename_construction.f90
! Purpose: Illustrate number-to-string conversions using internal files;
!          specifically, this program employs this technique to construct output
!          file-names based on the iteration-number in a hypothetical numerical
!          model.

program timestep_filename_construction
  implicit none
  character(40) :: auxString ! internal file (=string)
  integer :: i, numTimesteps = 10, speedFileID

  ! do is for looping over an integer interval (discussed soon)
  do i=1, numTimesteps
     ! write timestep into auxString
     write(auxString, '(i0)') i
     ! open file for writing, with custom filename
     open(newunit=speedFileID, &
          file="speed_" // trim(adjustl(auxString)) // ".dat", &
          action="write")

     ! here, we would have model-code, for computing the speed and writing
     ! it to file...

     close(speedFileID)
  end do
end program timestep_filename_construction

