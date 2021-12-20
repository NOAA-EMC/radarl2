
 SUBROUTINE EX_SETUP_RADAR

  use variable_define

  IMPLICIT NONE

! --------------------------------------------
! prepare radar data
! --------------------------------------------

  CALL EX_PREP_RADAR

! --------------------------------------------
! origin radar position
! --------------------------------------------

  CALL ORIG_RADAR

 END SUBROUTINE EX_SETUP_RADAR
