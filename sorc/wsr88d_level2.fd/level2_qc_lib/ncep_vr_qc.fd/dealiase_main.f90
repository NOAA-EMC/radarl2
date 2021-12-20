 SUBROUTINE Dealiase_Main
!
!=======================================================================
!  PURPOSE:
!     Main_Driver program is used to drive dealiasing program. All
!     dealiasing program don't need extra information, they utilize
!     aliased radial velocity to retrieval horizontal wind profile,
!     with the wind finish reference checking, then continuity checking.
!=======================================================================
!
!  Author   : Gong, Jiandong
!  Date     : 
!  Action   : Created.
!
!  History  :
!  ----------
!  Mar.27, 2007: Pengfei change it to a subroutine "Dealiase_Main"
!                for QC package.
!
!  Sep.25, 2007: Qin Xu and kang Nai introduced a two step dialiasing
!                agorithm to quality contorl the Doppler radar velocity
!                data. Which used the non_line Kapa function to get
!                the reference field and do unfolding job.
!
!-----------------------------------------------------------------------
   USE variable_define

   IMPLICIT NONE

!-----------------------------------------------------------------------

   WRITE (6, '(/,10X,A,/)') '*** Dealiasing Starting ***'

!-----------------------------------------------------------------------
!  [1.0] Set some count parameter's initial value.
!-----------------------------------------------------------------------

   count_no_select_circle=0
   count_toomuch_0Vr=0

!-----------------------------------------------------------------------
!  [2.0] Set up and read in radar data
!-----------------------------------------------------------------------

   CALL Setup_radar
   if ( no_qualify_job ) then
           write(*,*)  'no qualify job in setup_radar'
           RETURN
   end if

!-----------------------------------------------------------------------
!  [3.0] Set up quality control for noise remove.
!-----------------------------------------------------------------------

   CALL Setup_noise_remove

!-----------------------------------------------------------------------
!  [4.0] Select cycles at the standard height from the ground.
!-----------------------------------------------------------------------

   CALL setup_circle

   if ( no_qualify_job ) then
           write(*,*)  'no qualify job in select_radar'
           RETURN
   end if

!-----------------------------------------------------------------------
!  [5.0] build in wind profile and reference velocity
!-----------------------------------------------------------------------

   CALL Setup_Wind_profile

!-----------------------------------------------------------------------
!  [6.0] dealiasing starting
!-----------------------------------------------------------------------

   CALL Multi_check_driver
!-----------------------------------------------------------------------
!  [6.0] dealiasing starting
!-----------------------------------------------------------------------
   CALL classical_VAD 

   WRITE (6, '(/,10X,A,/)') '*** Dealiasing End ***'

 END SUBROUTINE Dealiase_Main
