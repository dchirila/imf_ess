! File: portable_real_kinds.f90
! Purpose: Demonstrate how to package the code for selecting the appropriate
!          kind-parameter for a desired floating-point precision.
! Disclaimer: An issue which remains to be solved is that the module is in the
!             same file as the program using it; for re-use of the module, it
!             should reside in a separate file (we finally demonstrate how to
!             solve this problem also in Chapter 4, when discussing
!             build-systems).

module RealKinds
  implicit none
  ! KIND-parameters for real-values
  integer, parameter :: &
       R_SP = selected_real_kind( 6, 37 ), &
       R_DP = selected_real_kind( 15, 307 ), &
       R_QP = selected_real_kind( 33, 4931 )
  ! Edit-descriptors for real-values
  character(len=*), parameter :: R_SP_FMT = "f0.6", &
       R_DP_FMT = "f0.15", R_QP_FMT = "f0.33"

contains
  ! Module-subprogram.
  subroutine printSupportedRealKinds()
    write(*,'(a)') "** START: printSupportedRealKinds **"
    if( R_SP > 0 ) then
       write(*,'(a,i0,a)') "single-prec. supported (kind=", R_SP, ")"
    else
       write(*,'(a,i0,a)') "single-prec. MISSING!  (kind=", R_SP, ")"
    end if

    if( R_DP > 0 ) then
       write(*,'(a,i0,a)') "double-prec. supported (kind=", R_DP, ")"
    else
       write(*,'(a,i0,a)') "double-prec. MISSING!  (kind=", R_DP, ")"
    end if

    if( R_QP > 0 ) then
       write(*,'(a,i0,a)') "quad-prec.   supported (kind=", R_QP, ")"
    else
       write(*,'(a,i0,a)') "quad-prec.   MISSING!  (kind=", R_QP, ")"
    end if
    write(*,'(a)') "** END: printSupportedRealKinds **"
  end subroutine printSupportedRealKinds
end module RealKinds

program portable_real_kinds
  use RealKinds, only : R_DP, R_QP, &
       R_DP_FMT, R_QP_FMT, &
       showFloatingPoingDiagnostics => printSupportedRealKinds
  implicit none

  real(R_DP) a
  real(R_QP) b

  a = sqrt(2.0_R_DP); b = sqrt(2.0_R_QP)

  call showFloatingPoingDiagnostics()

  write(*,'(a,1x,' // R_DP_FMT // ')') "sqrt(2) in    double-precision is", a
  write(*,'(a,1x,' // R_QP_FMT // ')') "sqrt(2) in quadruple-precision is", b
end program portable_real_kinds
