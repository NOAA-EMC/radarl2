      SUBROUTINE Extr_Dealiase_Main
!     ==================================================================
!     PURPOSE:
!       Main_Driver program is used to drive dealiasing program which
!       is special used to treat the VCP=121,221 case. The basement
!       field is caused by the new two step dealiase method.
!       ( this code is based on Gong's code).
!     ==================================================================
!     Author   : Kang Nai
!     Date     : 02/18/2008
!     Action   : Created.
!
!     Modify History  :
!     ------------------------------------------------------------------
      USE variable_define

      IMPLICIT NONE

!     ------------------------------------------------------------------

      WRITE (0, '(/,10X,A,/)') '*** Extro-Dealiasing Starting ***'

!     ------------------------------------------------------------------
!     [1.0] Set up and read in radar data
!     ------------------------------------------------------------------

      CALL ex_Setup_radar

!     ------------------------------------------------------------------
!     [2.0] Set up quality control for noise remove.
!     ------------------------------------------------------------------

      CALL Setup_noise_remove

!     ------------------------------------------------------------------
!     [3.0] dealiasing starting
!     ------------------------------------------------------------------

      CALL Multi_check_driver

      END SUBROUTINE Extr_Dealiase_Main
