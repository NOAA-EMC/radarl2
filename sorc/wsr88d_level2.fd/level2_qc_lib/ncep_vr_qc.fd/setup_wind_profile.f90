
SUBROUTINE SETUP_WIND_PROFILE

! ======================================================================
!   PURPOSE:
!     Get the horizontal wind field's profile by using:
!     slope_method;
!     bayes_method;
!     conti_method;
!     covar_method;
!
! ======================================================================
!
!   Author   : Gong, Jiandong et al.
!   Date     :
!   Action   : Created.
!
!   History  :
!  -----------
!  Sep. 25, 2007   Nai, Kang change the method to 0Vr method.
!
! ----------------------------------------------------------------------
  USE variable_define

  IMPLICIT NONE
 
  real :: a1,a2,a3,a4

! ----------------------------------------------------------------------
! 0Vr and the tendency method to get the wind profile
! ----------------------------------------------------------------------
  CALL tendency_method

! ----------------------------------------------------------------------
! wind profile quality check
! ----------------------------------------------------------------------
  call profile_continue_qc
  call profile_differ_qc

!-----------------------------------------------------------------------
! full the profile
!-----------------------------------------------------------------------
  call full_profile

! ----------------------------------------------------------------------
! write out wind profile
! ----------------------------------------------------------------------
! open(31,file='aprfl.dat',form='formatted')
! do ilevel=1,zlevel
!    if ( abs(ustor(ilevel))<spval .and.                             &
!         abs(vstor(ilevel))<spval ) then
!         a1 = ustor(ilevel)*ustor(ilevel)
!         a2 = vstor(ilevel)*vstor(ilevel)
!         a3 = a1+a2
!         if ( a3 .gt. 0.0 ) then
!              a4 = sqrt(a3)
!         else
!              a4 = 0.0
!         endif
!         a1 = hstor(ilevel)/1000.0
!         a2 = 0.0
!         write(31,'(4e15.5,i8,f5.1)') a1,ustor(ilevel),vstor(ilevel) &
!                                     ,a4,ilevel,a2
!    else
!         a4=spval
!    endif
! enddo
! close(31)

!------------------------------------------------------------------------------
! setup vad flag
!------------------------------------------------------------------------------
  call decide_vadflag

 END SUBROUTINE SETUP_WIND_PROFILE
