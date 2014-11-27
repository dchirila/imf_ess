! File: lbm2d_mrt_rb_v3.f90
!
! Purpose: Example program for solving the 2D incompressible Navier-Stokes
! equation (INSE), with a thermal component coupled using the Boussinesq
! approximation. For simulating the evolution of the (athermal) fluid, and of
! the temperature-field, we use the lattice Boltzmann method (LBM), with
! multi-relaxation-time (MRT) collision operators.
!
! Geometry: rectangular channel, with aspect-ratio
! $\gamma \equiv \frac{L_x}{L_y} = 2.0158$
!
! Boundary Conditions (BCs):
! - periodic domain-wrapping along X-direction (for both fluid- and
!   temperature-solvers)
! - fluid: no-slip (achieved through "bounce-back") at top and bottom walls
! - temperature: constant temperature (achieved through "anti-bounce-back") at
!   top and bottom walls; the lower wall is maintained at a higher temperature,
!   to allow convection to occur
!
! Initial Conditions (ICs):
! - fluid: at rest and without any pressure-gradients
! - temperature: linear temperature-profile (to speed-up the simulations), plus
!   a small perturbation near the center of the bottom wall, to break the
!   symmetry.
!
! Author: Dragos B. Chirila
!
! Version Information: The main difference between this version of the
! application (v3) and the previous one (v2) is that here we introduce
! preliminary support for OpenMP (actual relevant code-changes are in
! 'RBenardSimulation_class.f90', 'MrtSolverBoussinesq2D_class.f90', and
! 'Makefile').
!
! References:
! - [Rayleigh1916] "On convection currents in a horizontal layer of fluid, when
! the higher temperature is on the under side", Lord Rayleigh, 1916, Philosophical
! Magazine Series 6, 32(192)
! - [Wang2013] "Lattice Boltzmann simulations of thermal convective flows in two
! dimensions", J. Wang et al., 2013, Computers & Mathematics with Applications,
! 65(2)
! - [Shan19917] "Simulation of Rayleigh-Benard convection using a lattice
! Boltzmann method", 1997, Physical Review E, 55(3)

program lbm2d_mrt_rb_v3
  use RBenardSimulation_class
  implicit none

  type(RBenardSimulation) :: testSim

  call testSim%init( Ra=1900._RK, Pr=7.1_RK, &
       nY=62, simTime=50._RK, maxMach=0.3_RK, &
       numOutSlices=1000, outFilePrefix="rb_Ra_1900" )

  call testSim%run()

  call testSim%cleanup()
end program lbm2d_mrt_rb_v3
