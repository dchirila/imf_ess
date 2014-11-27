! File: file_io_demo_manual_unit_numbers.f90
! Purpose: Test-program, illustrating how we can perform file-I/O, using the
!          manual selection of unit-numbers.
! NOTE: The recommended (and easier) practice is to use automatic unit-numbers
!       (see file "file_io_demo_auto_unit_numbers.f90")!

program file_io_demo_manual_unit_numbers
  implicit none

  integer :: statCode
  real :: windUx=1.0, windUy=2.0, pressure=3.0

  ! assuming file "wind.dat" exists, open it for reading, selecting
  ! the value of 20 as unit-id; no error-handling
  open(unit=20, file="wind.dat", status="old", action="read")

  ! open file "pressure.dat" for writing (creating it if it does not
  ! exist, or deleting and re-creating it if it exists), selecting
  ! the value of 21 as unit-id; place in variable 'statCode' the
  ! result of the open-operation
  open(unit=21, file="pressure.dat", status="replace", &
       action="write", iostat=statCode)

  ! open a scratch-file, for storing some intermediate-result (which
  ! we need to read later), that would be too large to keep in memory;
  ! no error-handling
  open(unit=22, status="scratch", action="readwrite")

  ! ... some code to compute pressure ...
  read(20, *) windUx, windUy

  ! display on-screen the values read from the "wind.dat"-file
  write(*,'("windUx =", 1x, f0.8, 2x, "windUy =", 1x, f0.8)') &
       windUx, windUy

  ! write to scratch-file (here, only for illustration-purpose; this makes
  ! more sense if 'pressure' is a large array, which we would want to modify,
  ! or deallocate afterwards, to save memory)
  write(22, '(f10.6)') pressure ! write to scratch
  ! re-position file cursor at beginning of the scratch-file
  rewind 22
  ! ... after some time, re-load the 'pressure'-data from the scratch-file
  read(22, '(f10.6)') pressure

  ! write final data to "pressure.dat"-file
  write(21, '(f10.6)') pressure*2

  ! close all files
  close(20); close(21); close(22)
end program file_io_demo_manual_unit_numbers
