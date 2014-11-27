module MrtSolverBoussinesq2D_class
  use NumericKinds, only : IK, RK, swap
  use LbmConstantsMrtD2Q5
  use LbmConstantsMrtD2Q9
  implicit none

  type :: MrtSolverBoussinesq2D
     private
     ! parameters for the algorithm (not bound to the RB-setup)
     real(RK) :: mAlphaG, mAParam, mViscosity, mDiffusivity, &
          mTempColdWall, mTempHotWall, &
          ! for relaxation-matrices, we store only the non-zero part (= diagonals)
          mRelaxVecFluid(0:8), mRelaxVecTemp(0:4)

     ! internal model arrays
     ! NOTES: - last dimension is for 2-lattice alternation
     !        - 1st dimension: 0-8 = fluid, 9-13 = temp DFs
     real(RK), dimension(:,:,:,:), allocatable :: mDFs
     ! raw moments from which we can compute macroscopic fields; this is used
     ! mainly for simulation-output
     ! 0 ~ pressure| 1 ~ uX| 2 ~ uY| 3 ~ temp
     real(RK), dimension(:,:,:), allocatable :: mRawMacros

     integer(IK) :: mOld, mNew, & ! for tracking most recent lattice
          mNx, mNy ! mesh-size (received from 'sim'-class)

   contains
     private
     procedure, public :: init => initMrtSolverBoussinesq2D
     procedure, public :: advanceTime => advanceTimeMrtSolverBoussinesq2D
     procedure, public :: cleanup => cleanupMrtSolverBoussinesq2D
     procedure, public :: getRawMacros => getRawMacrosMrtSolverBoussinesq2D
     ! internal methods
     procedure :: calcLocalMomsMrtSolverBoussinesq2D
     procedure :: calcLocalEqMomsMrtSolverBoussinesq2D
  end type MrtSolverBoussinesq2D

contains
  function getRawMacrosMrtSolverBoussinesq2D( this ) result(macros)
    class(MrtSolverBoussinesq2D), intent(in) :: this
    real(RK), dimension(this%mNx, this%mNy, 0:3) :: macros
    macros = this%mRawMacros
  end function getRawMacrosMrtSolverBoussinesq2D

  subroutine initMrtSolverBoussinesq2D( this, nX, nY, tempColdWall, tempHotWall, &
       viscosity, diffusivity, alphaG, aParam, relaxVecFluid, relaxVecTemp )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    real(RK), intent(in) :: tempColdWall, tempHotWall, &
         viscosity, diffusivity, alphaG, aParam, &
         relaxVecFluid(0:8), relaxVecTemp(0:4)
    integer(IK), intent(in) :: nX, nY
    integer(IK) :: x, y, i ! dummy vars
    integer(IK), dimension(0:1) :: dest
    ! temporary moments-vars
    real(RK) :: fluidMoms(0:8), tempMoms(0:4), tempPerturbation

    ! copy argument-values internally
    this%mNx = nX; this%mNy = nY
    this%mTempColdWall = tempColdWall; this%mTempHotWall = tempHotWall
    this%mViscosity = viscosity; this%mDiffusivity = diffusivity
    this%mAlphaG = alphaG; this%mAParam = aParam
    this%mRelaxVecFluid = relaxVecFluid; this%mRelaxVecTemp = relaxVecTemp

    tempPerturbation=this%mTempHotWall/1.E5_RK

    ! get memory for model-state arrays (and Y-buffers)
    allocate( this%mDFs(0:13, 1:this%mNx, 0:(this%mNy+1), 0:1) )
    allocate( this%mRawMacros(this%mNx, this%mNy, 0:3) )

    ! initialize
    this%mDFs = 0._RK
    this%mRawMacros = 0._RK

    ! init tracking-vars for lattice-alternation
    this%mOld = 0; this%mNew = 1

    ! ICs for model's state-arrays
    do y=1, this%mNy
       do x=1, this%mNx
          ! reset moments-vectors
          fluidMoms = 0._RK; tempMoms = 0._RK
          ! Initialize pressure with steady-state (quadratic) profile, to avoid
          ! the initial oscillations.
          fluidMoms(0) = &
               (3._RK*this%mAlphaG)/(2._RK*this%mNy)*(y-0.5_RK)*(this%mNy+0.5_RK-y)

          ! Initialize temperature with steady-state (linear) profile, to save
          ! CPU-time. Also here, we insert small perturbation, to break the
          ! symmetry of the system (otherwise, the simulation is too stable).
          tempMoms(0) = 0.5_RK - (2._RK*y-1._RK)/(2._RK*this%mNy)
          if( (x == this%mNx/3+1) .and. (y == 2) ) then
             tempMoms(0) = tempMoms(0)+tempPerturbation
          end if

          ! map moments onto DFs...
          ! ...fluid
          do i=0, 8
             this%mDFs(i, x, y, this%mOld) = dot_product(M_INV_FLUID(:,i), fluidMoms)
          end do
          ! ...temp
          do i=0, 4
             this%mDFs(i+9, x, y, this%mOld) = dot_product(N_INV_TEMP(:,i), tempMoms)
          end do

          ! Fill buffers for bounce-back (for initial time-step)
          ! ...fluid
          do i=0, 8
             dest(0) = mod(x+EV_FLUID(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_FLUID(2, i)
             if( (dest(1) == 0) .or. (dest(1) == this%mNy+1) ) then
                this%mDFs(i, dest(0), dest(1), this%mOld) = this%mDFs(i, x, y, this%mOld)
             end if
          end do

          ! ...temp
          do i=0, 4
             dest(0) = mod(x+EV_TEMP(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_TEMP(2, i)
             if( (dest(1) == 0) .or. (dest(1) == this%mNy+1) ) then
                this%mDFs(i+9, dest(0), dest(1), this%mOld) = this%mDFs(i+9, x, y, this%mOld)
             end if
          end do

          ! save ICs
          this%mRawMacros(x, y, :) = [ fluidMoms(0:2), tempMoms(0) ]
       end do
    end do
  end subroutine initMrtSolverBoussinesq2D

  subroutine calcLocalMomsMrtSolverBoussinesq2D( this, x, y, fluidMoms, tempMoms )
    class(MrtSolverBoussinesq2D), intent(in) :: this
    integer(IK), intent(in) :: x, y
    real(RK), intent(out) :: fluidMoms(0:8), tempMoms(0:4)
    ! .............................
    integer(IK) :: i ! dummy index

    fluidMoms = 0._RK; tempMoms = 0._RK
    ! update fluid-moments
    do i=0, 8
       fluidMoms(i) = dot_product( M_FLUID(:,i), this%mDFs(0:8, x, y, this%mOld) )
    end do
    ! update temp-moments
    do i=0, 4
       tempMoms(i) = dot_product( N_TEMP(:,i), this%mDFs(9:13, x, y, this%mOld) )
    end do
  end subroutine calcLocalMomsMrtSolverBoussinesq2D

  subroutine calcLocalEqMomsMrtSolverBoussinesq2D( this, &
       dRho, uX, uY, temp, fluidEqMoms, tempEqMoms )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    real(RK), intent(in) :: dRho, uX, uY, temp
    real(RK), intent(out) :: fluidEqMoms(0:8), tempEqMoms(0:4)
    ! .............................
    ! fluid
    fluidEqMoms(0) = dRho
    fluidEqMoms(1) = uX
    fluidEqMoms(2) = uY
    fluidEqMoms(3) = -2._RK*dRho + 3._RK*(uX**2+uY**2)
    fluidEqMoms(4) = uX**2-uY**2
    fluidEqMoms(5) = uX*uY
    fluidEqMoms(6) = -uX
    fluidEqMoms(7) = -uY
    fluidEqMoms(8) = -fluidEqMoms(3) - dRho
    ! temp
    tempEqMoms(0) = temp
    tempEqMoms(1) = temp*uX
    tempEqMoms(2) = temp*uY
    tempEqMoms(3) = this%mAParam*temp
    tempEqMoms(4) = 0._RK
  end subroutine calcLocalEqMomsMrtSolverBoussinesq2D

  ! advance solver-state by one time-step (core LBM-algorithm)
  subroutine advanceTimeMrtSolverBoussinesq2D( this )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    ! local vars
    integer(IK) :: x, y, i, old, new ! dummy indices
    integer(IK), dimension(0:1) :: dest
    real(RK) :: fluidMoms(0:8), tempMoms(0:4), &
         fluidEqMoms(0:8), tempEqMoms(0:4)

    ! initializations
    dest = 0; fluidMoms = 0._RK; tempMoms = 0._RK
    fluidEqMoms = 0._RK; tempEqMoms = 0._RK
    old = this%mOld; new = this%mNew

    do y=1, this%mNy
       do x=1, this%mNx
          call this%calcLocalMomsMrtSolverBoussinesq2D(x, y, fluidMoms, tempMoms)

          ! add 1st-half of force term (Strang splitting)
          fluidMoms(2) = fluidMoms(2) + this%mAlphaG*0.5_RK*tempMoms(0)

          ! save moments related to output
          this%mRawMacros(x, y, :) = &
               [ fluidMoms(0), fluidMoms(1), fluidMoms(2), tempMoms(0) ]

          call this%calcLocalEqMomsMrtSolverBoussinesq2D( dRho=fluidMoms(0), &
               uX=fluidMoms(1), uY=fluidMoms(2), temp=tempMoms(0), &
               fluidEqMoms=fluidEqMoms, tempEqMoms=tempEqMoms )

          ! collision (in moment-space)
          fluidMoms = fluidMoms - this%mRelaxVecFluid * (fluidMoms - fluidEqMoms)
          tempMoms  = tempMoms  - this%mRelaxVecTemp * (tempMoms - tempEqMoms)

          ! add 2nd-half of force term (Strang splitting)
          fluidMoms(2) = fluidMoms(2) + this%mAlphaG*0.5_RK*tempMoms(0)

          ! map moments back onto DFs...
          ! ...fluid
          do i=0, 8
             this%mDFs(i, x, y, old) = dot_product( M_INV_FLUID(:, i), fluidMoms )
          end do
          ! ...temp
          do i=0, 4
             this%mDFs(i+9, x, y, old) = dot_product( N_INV_TEMP(:, i), tempMoms )
          end do

          ! stream to new array...
          ! ...fluid
          do i=0, 8
             dest(0) = mod(x+EV_FLUID(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_FLUID(2, i)
             ! STREAM (also storing runaway DFs in Y-buffer space)
             this%mDFs(i, dest(0), dest(1), new) = this%mDFs(i, x, y, old)
             if( dest(1) == 0 ) then
                if( EV_FLUID(2, i) /= 0 ) then
                   ! apply bounce-back @bottom
                   this%mDFs(OPPOSITE_FLUID(i), x, y, new) = &
                        this%mDFs(i, dest(0), dest(1), old)
                end if
             elseif( dest(1) == this%mNy+1 ) then
                if( EV_FLUID(2, i) /= 0 ) then
                   ! apply bounce-back @top
                   this%mDFs(OPPOSITE_FLUID(i), x, y, new) = &
                        this%mDFs(i, dest(0), dest(1), old)
                end if
             end if
          end do
          ! ...temp
          do i=0, 4
             dest(0) = mod(x+EV_TEMP(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_TEMP(2, i)
             ! STREAM (also storing runaway DFs in Y-buffer space)
             this%mDFs(i+9, dest(0), dest(1), new) = this%mDFs(i+9, x, y, old)
             if( dest(1) == 0 ) then
                ! apply anti-bounce-back @bottom
                this%mDFs(OPPOSITE_TEMP(i)+9, x, y, new) = &
                     -this%mDFs(i+9, dest(0), dest(1), old) + &
                     2._RK*sqrt(3._RK)*this%mDiffusivity*this%mTempHotWall
             elseif( dest(1) == this%mNy+1 ) then
                ! apply anti-bounce-back @top
                this%mDFs(OPPOSITE_TEMP(i)+9, x, y, new) = &
                     -this%mDFs(i+9, dest(0), dest(1), old) + &
                     2._RK*sqrt(3._RK)*this%mDiffusivity*this%mTempColdWall
             end if
          end do
       end do
    end do

    ! swap 'pointers' (for lattice-alternation)
    call swap( this%mOld, this%mNew )
  end subroutine advanceTimeMrtSolverBoussinesq2D

  subroutine cleanupMrtSolverBoussinesq2D( this )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    deallocate( this%mDFs, this%mRawMacros ) ! release memory
  end subroutine cleanupMrtSolverBoussinesq2D
end module MrtSolverBoussinesq2D_class
