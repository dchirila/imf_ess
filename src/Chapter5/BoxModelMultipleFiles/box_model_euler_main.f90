! File: box_model_euler_main.f90
! Purpose: Example program for simulating the evolution of the climate
!          box-model.
! NOTES: - The actual physics is encoded into the function 'dModelState'
!          (file "ModelState_class.f90"), which contains the RHS of the
!          evolution-equations for the model-variables.
!        - To change the precision of the floating-point calculations, the
!          'RK'-parameter (in file "NumericKinds.f90") can be set to 'R_SP',
!          'R_DP', or 'R_QP' (for single-, double-, and quad-precision,
!          respectively). WARNING: when changing this, 'RK_FMT' should also be
!          changed accordingly!

program box_model_euler
  use PhysicsConstants
  use ModelConstants
  use ModelState_class
  implicit none

  integer :: i, outFileID
  type(ModelState) :: stateSim1E, statePerturbation

  ! Perturbation to superimpose over equilibrium state.
  statePerturbation = ModelState( TocS = 0., TocM = 0., &
       TocN = 0., TocD = 0., &
       SocS = 0., SocM = 0., &
       SocN = -0.7, SocD = 0., &
       TatS = 0., TatM = 0., TatN = 0. )

  stateSim1E = ModelState( TocS = 4.77740431, TocM = 24.42876625, &
       TocN = 2.66810894, TocD = 2.67598915, &
       SocS = 34.40753555, SocM = 35.62585068, &
       SocN = 34.92513657, SocD = 34.91130066, &
       TatS = 4.67439556, TatM = 23.30437851, TatN = 0.94061828) + statePerturbation

  ! prepare for output
  open(newunit=outFileID, file="box_model_euler.out", &
       form="formatted", status="replace")

  ! write initial conditions
  write(outFileID, '(14('// RK_FMT // ', 1x))') 0._RK, stateSim1E%getCurrModelState()

  do i=1, NO_T_STEP
     ! Euler-forward step
     stateSim1E = stateSim1E + DTS*dModelState(stateSim1E)

     call stateSim1E%preventOceanFreezing()

     ! Conditional OUTPUT-writing
     if( mod(i-1, OUTPUT_FREQUENCY) == 0 ) then
        write(outFileID, '(14('// RK_FMT // ', 1x))') i*DT_IN_YEARS, &
             stateSim1E%getCurrModelState()
     end if
  end do
  close(outFileID) ! Clean-up for output
end program box_model_euler
