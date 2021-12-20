
 SUBROUTINE SETUP_RADAR

  use variable_define

  IMPLICIT NONE

! --------------------------------------------
! prepare radar data
! --------------------------------------------

  CALL PREP_RADAR

! --------------------------------------------
! origin radar position
! --------------------------------------------

  CALL ORIG_RADAR

 END SUBROUTINE SETUP_RADAR
