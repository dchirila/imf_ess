! File: file_io_example.f90
! Purpose: Read three integers from file "data.in", and write them (in reverse
!          order) to file "data.out".
! Pre-condition: File "data.in" has to be created externally, and should contain
!                at least 3 integers.
program file_io_example
  implicit none
  integer a, b, c
  integer inFileID, outFileID

  ! link files on disk with file-descriptor
  open(newunit=inFileID, file="data.in")
  open(newunit=outFileID, file="data.out")

  read(inFileID,*) a, b, c ! read from input-file

  ! display numbers on terminal
  write(*,*) "a=", a, ", b=", b, ", c=", c

  write(outFileID,*) c, b, a ! write to output-file

  close(inFileID); close(outFileID) ! close files
end program file_io_example

