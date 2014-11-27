! File: file_io_demo_auto_unit_numbers.f90
! Purpose: Test-program, illustrating how we can perform file-I/O, using the
!          automatic selection of unit-numbers.
! NOTES:
! - except the improved mechanism for assigning unit-numbers, the program is
!   basically identical to the one in "file_io_demo_manual_unit_numbers.f90"
! - because "newunit" is a Fortran 2008 feature, this program requires an
!   up-to-date compiler (e.g. gfortran-4.8 or higher)

program file_io_demo_auto_unit_numbers
  implicit none

  integer :: statCode, windFileID, pressureFileID, scratchFileID
  real :: windUx=1.0, windUy=2.0, pressure=3.0
  ! assuming file "wind.dat" exists, open it for reading, and store an
  ! (automatically-acquired) unit-number in variable 'windFileID'; no
  ! error-handling
  open(newunit=windFileID, file="wind.dat", status="old", &
       action="read")

  ! open file "pressure.dat" for writing (creating it if it does not
  ! exist, or deleting and re-creating it if it exists), while storing
  ! the (automatically-acquired) unit-number in variable 'pressureFileID';
  ! place in variable 'statCode' the result of the open-operation
  open(newunit=pressureFileID, file="pressure.dat", status="replace", &
       action="write", iostat=statCode)

  ! open a scratch-file, storing the (automatically-acquired) unit-number
  ! in variable 'scratchFileID'; no error-handling
  open(newunit=scratchFileID, status="scratch", action="readwrite")

  ! ... some code to compute pressure ...
  read(windFileID, *) windUx, windUy

  ! display on-screen the values read from the "wind.dat"-file
  write(*,'("windUx =", 1x, f0.8, 2x, "windUy =", 1x, f0.8)') &
       windUx, windUy

  ! write to scratch-file (here, only for illustration-purpose; this makes
  ! more sense if 'pressure' is a large array, which we would want to modify,
  ! or deallocate afterwards, to save memory)
  write(scratchFileID, '(f10.6)') pressure ! write to scratch
  ! re-position file cursor at beginning of the scratch-file
  rewind scratchFileID
  ! ... after some time, re-load the 'pressure'-data from the scratch-file
  read(scratchFileID, '(f10.6)') pressure

  ! write final data to "pressure.dat"-file
  write(pressureFileID, '(f10.6)') pressure*2

  ! close all files
  close(windFileID); close(pressureFileID); close(scratchFileID)
end program file_io_demo_auto_unit_numbers
