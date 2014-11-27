program mixed_type_edit_descriptors
  implicit none
  integer :: testInt = 1297, i
  real :: testReal = 12.9756, x(4), y
  real, parameter :: ALPHA = 1.0e3, BETA = -1.0e3
  integer :: seedArray(16)

  ! Quick method to fill seedArray
  call date_and_time(values=seedArray(1:8))
  call date_and_time(values=seedArray(9:16))
  ! Seed the RNG
  call random_seed( put=seedArray )

  print '("testInt = ", i5, " m/2")', testInt
  print '("testInt = ", i5.5, " m/2")', testInt
  print '("testInt = ", i0, " m/2")', testInt

  print '("testInt = ", b16, " m/2")', testInt
  print '("testInt = ", b16.16, " m/2")', testInt
  print '("testInt = ", b0, " m/2")', testInt

  print '("testInt = ", o5, " m/2")', testInt
  print '("testInt = ", o5.5, " m/2")', testInt
  print '("testInt = ", o0, " m/2")', testInt

  print '("testInt = ", z4, " m/2")', testInt
  print '("testInt = ", z4.4, " m/2")', testInt
  print '("testInt = ", z0, " m/2")', testInt

  print '("testReal = ", f0.6, " m/2")', testReal

  do i=1,50
     call random_number( x )
     !print*, (x(j), j=1,4)
     y = ALPHA*x(1)-ALPHA*x(2)+BETA*x(3)-BETA*x(4)
     print '(&
          &f14.4, " ",&
          &e14.4, " ",&
          &e14.4e1, " ",&
          &en14.4, " ",&
          &en14.4e1, " ",&
          &es14.4, " ",&
          &es14.4e1, "  ",&
          &f0.4)',&
          y, y, y, y,&
          y, y, y, y
  enddo

end program mixed_type_edit_descriptors
